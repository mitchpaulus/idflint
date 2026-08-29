using System;
using System.Collections.Generic;
using System.Globalization;
using Microsoft.Data.Sqlite;

namespace dotnet
{
    /// <summary>
    /// Serves IdfObject definitions from a per-version SQLite database
    /// (see mitchpaulus/idf-default-objects). Objects are built lazily and cached,
    /// so a lint run only pays for the object types the file actually uses.
    /// </summary>
    public class IdfObjectProvider : IDisposable
    {
        private readonly SqliteConnection _connection;
        private readonly Dictionary<string, long> _objectTypeIds = new Dictionary<string, long>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, IdfObject> _cache = new Dictionary<string, IdfObject>(StringComparer.OrdinalIgnoreCase);

        private Dictionary<string, HashSet<string>> _referenceClassList;
        private List<string> _requiredObjectNames;

        public string Version { get; }

        public IdfObjectProvider(string dbPath, string version = null)
        {
            Version = version;
            _connection = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly");
            _connection.Open();

            using var command = _connection.CreateCommand();
            command.CommandText = "SELECT id, name FROM object_type";
            using var reader = command.ExecuteReader();
            while (reader.Read()) _objectTypeIds[reader.GetString(1)] = reader.GetInt64(0);
        }

        /// <summary>
        /// Opens the provider for the version named in an IDF file's Version object,
        /// downloading the matching database on first use. A null or unknown version
        /// falls back to the nearest available one with a warning.
        /// </summary>
        public static IdfObjectProvider ForVersion(string requestedVersion)
        {
            string resolved = IdfDataStore.ResolveVersion(requestedVersion, out bool exact);
            if (!exact && !string.IsNullOrWhiteSpace(requestedVersion))
            {
                Console.Error.WriteLine($"idflint: no object data for EnergyPlus version '{requestedVersion.Trim()}', using {resolved}");
            }

            string path = IdfDataStore.GetDatabasePath(resolved);
            return new IdfObjectProvider(path, resolved);
        }

        public bool ContainsKey(string objectTypeName) => _objectTypeIds.ContainsKey(objectTypeName);

        public IdfObject GetIdfObject(string objectTypeName)
        {
            if (_cache.TryGetValue(objectTypeName, out IdfObject cached)) return cached;

            if (!_objectTypeIds.TryGetValue(objectTypeName, out long objectTypeId))
            {
                throw new ArgumentException($"Unknown object type '{objectTypeName}'.");
            }

            IdfObject idfObject = BuildIdfObject(objectTypeId);
            _cache[objectTypeName] = idfObject;
            return idfObject;
        }

        /// <summary>
        /// Object types that must appear in every model (\required-object in the IDD).
        /// </summary>
        public IReadOnlyList<string> RequiredObjectNames
        {
            get
            {
                if (_requiredObjectNames != null) return _requiredObjectNames;

                _requiredObjectNames = new List<string>();
                using var command = _connection.CreateCommand();
                command.CommandText = "SELECT name FROM object_type WHERE is_required = 1";
                using var reader = command.ExecuteReader();
                while (reader.Read()) _requiredObjectNames.Add(reader.GetString(0));
                return _requiredObjectNames;
            }
        }

        /// <summary>
        /// Key: reference class list name (\reference-class-name in the IDD).
        /// Value: the object type names whose class name belongs to that list.
        /// </summary>
        public Dictionary<string, HashSet<string>> ReferenceClassList
        {
            get
            {
                if (_referenceClassList != null) return _referenceClassList;

                _referenceClassList = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
                using var command = _connection.CreateCommand();
                command.CommandText = @"
                    SELECT ln.name, o.name
                    FROM field_list fl
                    JOIN field f ON f.id = fl.field_id
                    JOIN object_type o ON o.id = f.object_type_id
                    JOIN list_kind lk ON lk.id = fl.kind_id
                    JOIN list_name ln ON ln.id = fl.list_name_id
                    WHERE lk.name = 'reference_class_name'";
                using var reader = command.ExecuteReader();
                while (reader.Read())
                {
                    string listName = reader.GetString(0);
                    if (!_referenceClassList.TryGetValue(listName, out var typeNames))
                    {
                        typeNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                        _referenceClassList[listName] = typeNames;
                    }
                    typeNames.Add(reader.GetString(1));
                }
                return _referenceClassList;
            }
        }

        private IdfObject BuildIdfObject(long objectTypeId)
        {
            IdfObject idfObject = new IdfObject();
            int? extensibleSize = null;
            int? extMaxItems = null;

            using (var command = _connection.CreateCommand())
            {
                command.CommandText = @"
                    SELECT name, min_fields, extensible_size, ext_max_items, obsolete, is_unique, is_required
                    FROM object_type WHERE id = $id";
                command.Parameters.AddWithValue("$id", objectTypeId);
                using var reader = command.ExecuteReader();
                if (!reader.Read()) throw new ArgumentException($"Object type id {objectTypeId} not found.");

                idfObject.Name = reader.GetString(0);
                idfObject.MinNumberOfFields = reader.IsDBNull(1) ? (int?)null : reader.GetInt32(1);
                extensibleSize = reader.IsDBNull(2) ? (int?)null : reader.GetInt32(2);
                extMaxItems = reader.IsDBNull(3) ? (int?)null : reader.GetInt32(3);
                idfObject.Obsolete = !reader.IsDBNull(4);
                idfObject.Unique = reader.GetInt64(5) != 0;
                idfObject.Required = reader.GetInt64(6) != 0;
            }

            idfObject.Extensible = extensibleSize != null;
            idfObject.ExtensibleCountSize = extensibleSize ?? 0;

            Dictionary<long, IdfField> fieldsById = new Dictionary<long, IdfField>();

            using (var command = _connection.CreateCommand())
            {
                command.CommandText = @"
                    SELECT id, name, field_name, field_type, is_extension, required,
                           default_string, default_number,
                           minimum, minimum_exclusive, maximum, maximum_exclusive,
                           autosizable, autocalculatable, units
                    FROM field WHERE object_type_id = $id
                    ORDER BY is_extension, position";
                command.Parameters.AddWithValue("$id", objectTypeId);
                using var reader = command.ExecuteReader();
                while (reader.Read())
                {
                    IdfField field = new IdfField
                    {
                        Name = !reader.IsDBNull(2) ? reader.GetString(2) : reader.GetString(1),
                        AlphaNumeric = !reader.IsDBNull(3) && reader.GetString(3) == "n"
                            ? IdfFieldAlphaNumeric.Numeric
                            : IdfFieldAlphaNumeric.Alpha,
                        Required = reader.GetInt64(5) != 0,
                        AutoSizeable = reader.GetInt64(12) != 0,
                        AutoCalculatable = reader.GetInt64(13) != 0,
                        Units = reader.IsDBNull(14) ? "" : reader.GetString(14),
                        Keys = new HashSet<string>(StringComparer.OrdinalIgnoreCase),
                    };

                    if (!reader.IsDBNull(6)) field.Default = reader.GetString(6);
                    else if (!reader.IsDBNull(7)) field.Default = reader.GetDouble(7).ToString("G10", CultureInfo.InvariantCulture);

                    if (!reader.IsDBNull(8))
                    {
                        field.Minimum = reader.GetDouble(8);
                        field.MinType = reader.GetInt64(9) != 0 ? IdfFieldMinMaxType.Exclusive : IdfFieldMinMaxType.Inclusive;
                    }

                    if (!reader.IsDBNull(10))
                    {
                        field.Maximum = reader.GetDouble(10);
                        field.MaxType = reader.GetInt64(11) != 0 ? IdfFieldMinMaxType.Exclusive : IdfFieldMinMaxType.Inclusive;
                    }

                    bool isExtension = reader.GetInt64(4) != 0;
                    if (isExtension) idfObject.ExtensionFields.Add(field);
                    else idfObject.Fields.Add(field);

                    fieldsById[reader.GetInt64(0)] = field;
                }
            }

            // Choice values (alpha fields only; numeric fields express Autosize and the
            // like through the autosizable/autocalculatable flags instead).
            using (var command = _connection.CreateCommand())
            {
                command.CommandText = @"
                    SELECT fc.field_id, fc.value
                    FROM field_choice fc
                    JOIN field f ON f.id = fc.field_id
                    WHERE f.object_type_id = $id AND fc.is_numeric = 0 AND f.field_type IS NOT 'n'";
                command.Parameters.AddWithValue("$id", objectTypeId);
                using var reader = command.ExecuteReader();
                while (reader.Read())
                {
                    string value = reader.GetString(1);
                    if (string.IsNullOrEmpty(value)) continue;
                    fieldsById[reader.GetInt64(0)].Keys.Add(value);
                }
            }

            using (var command = _connection.CreateCommand())
            {
                command.CommandText = @"
                    SELECT fl.field_id, lk.name, ln.name
                    FROM field_list fl
                    JOIN field f ON f.id = fl.field_id
                    JOIN list_kind lk ON lk.id = fl.kind_id
                    JOIN list_name ln ON ln.id = fl.list_name_id
                    WHERE f.object_type_id = $id
                    ORDER BY fl.position";
                command.Parameters.AddWithValue("$id", objectTypeId);
                using var reader = command.ExecuteReader();
                while (reader.Read())
                {
                    IdfField field = fieldsById[reader.GetInt64(0)];
                    string kind = reader.GetString(1);
                    string listName = reader.GetString(2);
                    switch (kind)
                    {
                        case "object_list": field.ObjectList.Add(listName); break;
                        case "reference": field.ReferenceList.Add(listName); break;
                        case "reference_class_name": field.ReferenceClassList.Add(listName); break;
                        // external_list values are not checked by the linter.
                    }
                }
            }

            if (!idfObject.Extensible)
            {
                idfObject.TotalNumberOfDefinedFields = idfObject.Fields.Count;
            }
            else if (extMaxItems != null)
            {
                idfObject.TotalNumberOfDefinedFields = idfObject.Fields.Count + extMaxItems.Value * idfObject.ExtensionFields.Count;
            }
            else
            {
                idfObject.TotalNumberOfDefinedFields = int.MaxValue;
            }

            return idfObject;
        }

        public void Dispose() => _connection.Dispose();
    }
}
