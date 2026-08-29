using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using dotnet.checks;

namespace dotnet
{
    public class IdfLinter
    {
        private readonly TextReader _reader;
        private IdfObjectProvider _provider;

        public IdfLinter(TextReader reader) => _reader = reader;

        public IdfLinter(string idf) => _reader = new StringReader(idf);

        public IdfLinter(string idf, IdfObjectProvider provider)
        {
            _reader = new StringReader(idf);
            _provider = provider;
        }

        /// <summary>
        /// Groups object indexes by type name, materializing each distinct type name
        /// string only once via the span alternate lookup.
        /// </summary>
        public static Dictionary<string, List<int>> GroupByType(ParsedIdf parsed)
        {
            Dictionary<string, List<int>> byType = new Dictionary<string, List<int>>(StringComparer.OrdinalIgnoreCase);
            var lookup = byType.GetAlternateLookup<ReadOnlySpan<char>>();
            for (int o = 0; o < parsed.ObjectCount; o++)
            {
                ReadOnlySpan<char> type = parsed.TypeSpan(parsed.Object(o));
                if (!lookup.TryGetValue(type, out List<int> list))
                {
                    list = new List<int>();
                    byType[type.ToString()] = list;
                }
                list.Add(o);
            }
            return byType;
        }

        /// <summary>
        /// Opens the object database matching the file's Version object (downloading
        /// it on first use). No-op if a provider was already supplied or resolved.
        /// </summary>
        private void EnsureProvider(ParsedIdf parsed, Dictionary<string, List<int>> byType)
        {
            if (_provider != null) return;

            string versionText = null;
            if (byType.TryGetValue("Version", out List<int> versionObjects) && versionObjects.Count > 0)
            {
                RawObject versionObject = parsed.Object(versionObjects[0]);
                if (versionObject.FieldCount > 0) versionText = parsed.FieldText(versionObject.FirstField);
            }

            _provider = IdfObjectProvider.ForVersion(versionText);
        }

        public List<IdfError> Lint()
        {
            List<IdfError> errors = new List<IdfError>();

            ParsedIdf parsed = IdfSourceParser.Parse(_reader.ReadToEnd());

            errors.AddRange(parsed.Errors);

            Dictionary<string, List<int>> byType = GroupByType(parsed);

            EnsureProvider(parsed, byType);

            foreach (string unknownTypeName in byType.Keys.Where(key => !_provider.ContainsKey(key)).ToList())
            {
                foreach (int objectIndex in byType[unknownTypeName])
                {
                    errors.Add(new ObjectTypeNotFoundError(parsed.ObjectPosition(parsed.Object(objectIndex)), unknownTypeName));
                }
                // Don't check any of the fields if we don't know what the object is.
                byType.Remove(unknownTypeName);
            }

            foreach (KeyValuePair<string, List<int>> pair in byType)
            {
                IdfObject idfObject = _provider.GetIdfObject(pair.Key);
                foreach (int objectIndex in pair.Value)
                {
                    RawObject obj = parsed.Object(objectIndex);
                    idfObject.FieldChecks(parsed, obj, errors);
                }
            }

            ReferenceListResult referenceListResult = GetReferenceLists(parsed, byType);

            errors.AddRange(referenceListResult.Errors);

            CheckObjectListReferences(parsed, byType, referenceListResult.ReferenceList, errors);

            foreach (string requiredObjectName in _provider.RequiredObjectNames)
            {
                if (!byType.TryGetValue(requiredObjectName, out List<int> objectInstances) || objectInstances == null || objectInstances.Count == 0)
                {
                    errors.Add(new RequiredObjectTypeNotFoundError(requiredObjectName));
                }
            }

            string[] designDayObjectNames =
            {
                "SizingPeriod:DesignDay",
                "SizingPeriod:WeatherFileDays",
                "SizingPeriod:WeatherFileConditionType"
            };

            bool hasDesignDay = designDayObjectNames.Any(name =>
                byType.TryGetValue(name, out List<int> contexts) && contexts != null && contexts.Count > 0);

            bool hasRunPeriod = byType.TryGetValue("RunPeriod", out List<int> runPeriodObjects) &&
                                runPeriodObjects != null && runPeriodObjects.Count > 0;

            if (!hasDesignDay && !hasRunPeriod)
            {
                errors.Add(new MissingDesignDaysAndRunPeriodsError());
            }

            if (!byType.TryGetValue("Timestep", out List<int> timestepObjects) ||
                timestepObjects == null || timestepObjects.Count == 0)
            {
                errors.Add(new MissingTimestepError());
            }

            return errors;
        }

        /// <summary>
        /// Fields declared with \object-list must name something present in one of the
        /// referenced lists (or reference class lists).
        /// </summary>
        private void CheckObjectListReferences(ParsedIdf parsed, Dictionary<string, List<int>> byType,
            Dictionary<string, HashSet<string>> referenceList, List<IdfError> errors)
        {
            foreach (KeyValuePair<string, List<int>> pair in byType)
            {
                IdfObject idfObject = _provider.GetIdfObject(pair.Key);
                foreach (int objectIndex in pair.Value)
                {
                    RawObject obj = parsed.Object(objectIndex);
                    for (int k = 0; k < obj.FieldCount; k++)
                    {
                        IdfField expectedField = idfObject.ExpectedFieldAt(k);
                        if (expectedField == null) break;
                        if (expectedField.ObjectList.Count == 0) continue;

                        int fieldIndex = obj.FirstField + k;
                        ReadOnlySpan<char> value = parsed.FieldSpan(fieldIndex);

                        // It's not an error if the field is empty and not required.
                        if (value.IsEmpty && !expectedField.Required) continue;

                        bool found = false;
                        foreach (string objectListType in expectedField.ObjectList)
                        {
                            if (InReferenceList(referenceList, objectListType, value) || InReferenceClassList(objectListType, value))
                            {
                                found = true;
                                break;
                            }
                        }

                        if (!found)
                        {
                            errors.Add(new FieldNotFoundInReferenceListError(parsed.FieldPosition(fieldIndex), value.ToString(), expectedField.ObjectList));
                        }
                    }
                }
            }
        }

        private static bool InReferenceList(Dictionary<string, HashSet<string>> referenceList, string objectListType, ReadOnlySpan<char> foundField)
        {
            return referenceList.TryGetValue(objectListType, out HashSet<string> names) &&
                   names.GetAlternateLookup<ReadOnlySpan<char>>().Contains(foundField);
        }

        // Type pairs EnergyPlus allows to share a name even though both feed the same
        // combined reference list. A Space named identically to its Zone is explicitly
        // supported (EnergyPlus itself auto-creates such spaces).
        private static readonly HashSet<(string, string)> AllowedSharedNameTypePairs = new HashSet<(string, string)>
        {
            ("SPACE", "ZONE"),
        };

        private static bool IsAllowedSharedName(string typeA, string typeB)
        {
            typeA = typeA.ToUpperInvariant();
            typeB = typeB.ToUpperInvariant();
            if (typeA == typeB) return false;
            var pair = string.CompareOrdinal(typeA, typeB) <= 0 ? (typeA, typeB) : (typeB, typeA);
            return AllowedSharedNameTypePairs.Contains(pair);
        }

        public bool InReferenceClassList(string objectListType, ReadOnlySpan<char> foundField) =>
            _provider.ReferenceClassList.TryGetValue(objectListType, out HashSet<string> typeNames) &&
            typeNames.GetAlternateLookup<ReadOnlySpan<char>>().Contains(foundField);

        /// <summary>
        /// Build up a Dictionary data structure for reference lists.
        /// Key: Reference List name from the IDD. Example: 'ScheduleNames'
        /// Value: List of possible names for that reference.
        /// </summary>
        public ReferenceListResult GetReferenceLists(ParsedIdf parsed, Dictionary<string, List<int>> byType)
        {
            Dictionary<string, HashSet<string>> referenceListDictionary = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
            // Reference list name -> contributed name -> object types that contributed it.
            // A repeated name is a duplicate unless the two contributing types are a pair
            // EnergyPlus allows to share names (see AllowedSharedNameTypePairs).
            Dictionary<string, Dictionary<string, HashSet<string>>> contributors = new Dictionary<string, Dictionary<string, HashSet<string>>>(StringComparer.OrdinalIgnoreCase);
            List<IdfError> errors = new List<IdfError>();

            EnsureProvider(parsed, byType);

            foreach (KeyValuePair<string, List<int>> pair in byType)
            {
                if (!_provider.ContainsKey(pair.Key)) continue;
                IdfObject idfObject = _provider.GetIdfObject(pair.Key);

                foreach (int objectIndex in pair.Value)
                {
                    RawObject obj = parsed.Object(objectIndex);
                    for (int k = 0; k < obj.FieldCount; k++)
                    {
                        IdfField expectedField = idfObject.ExpectedFieldAt(k);
                        if (expectedField == null) break;
                        if (expectedField.ReferenceList.Count == 0) continue;

                        int fieldIndex = obj.FirstField + k;
                        ReadOnlySpan<char> valueSpan = parsed.FieldSpan(fieldIndex);
                        if (valueSpan.IsEmpty) continue;

                        // One string per distinct name; every set stores the same instance.
                        string name = null;

                        // Add the field text to the reference list. See \reference in the IDD.
                        foreach (string refList in expectedField.ReferenceList)
                        {
                            if (!referenceListDictionary.TryGetValue(refList, out HashSet<string> referenceNames))
                            {
                                referenceNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                                referenceListDictionary[refList] = referenceNames;
                            }

                            if (!contributors.TryGetValue(refList, out Dictionary<string, HashSet<string>> namesToTypes))
                            {
                                namesToTypes = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
                                contributors[refList] = namesToTypes;
                            }

                            if (namesToTypes.GetAlternateLookup<ReadOnlySpan<char>>().TryGetValue(valueSpan, out HashSet<string> contributingTypes))
                            {
                                if (contributingTypes.Any(contributingType => !IsAllowedSharedName(contributingType, pair.Key)))
                                {
                                    errors.Add(new DuplicateNameInReferenceListError(parsed.FieldPosition(fieldIndex), valueSpan.ToString(), refList));
                                }
                            }
                            else
                            {
                                name ??= valueSpan.ToString();
                                contributingTypes = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                                namesToTypes[name] = contributingTypes;
                            }

                            name ??= valueSpan.ToString();
                            referenceNames.Add(name);
                            contributingTypes.Add(pair.Key);
                        }
                    }
                }
            }

            AddDefaultSpaces(parsed, byType, referenceListDictionary);

            return new ReferenceListResult(referenceListDictionary, errors);
        }

        private void AddDefaultSpaces(ParsedIdf parsed, Dictionary<string, List<int>> byType, Dictionary<string, HashSet<string>> referenceListDictionary)
        {
            if (!byType.TryGetValue("Zone", out List<int> zoneIndexes) || zoneIndexes.Count == 0) return;

            IdfObject zoneObject = _provider.GetIdfObject("Zone");
            HashSet<string> zoneNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

            foreach (int zoneIndex in zoneIndexes)
            {
                RawObject zone = parsed.Object(zoneIndex);
                if (zoneObject.TryGetFieldValue(parsed, zone, "Name", out string zoneName) &&
                    !string.IsNullOrWhiteSpace(zoneName))
                {
                    zoneNames.Add(zoneName);
                }
            }

            if (zoneNames.Count == 0) return;

            HashSet<string> zonesWithSpaces = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            if (byType.TryGetValue("Space", out List<int> spaceIndexes) && spaceIndexes.Count > 0)
            {
                IdfObject spaceObject = _provider.GetIdfObject("Space");
                foreach (int spaceIndex in spaceIndexes)
                {
                    RawObject space = parsed.Object(spaceIndex);
                    if (spaceObject.TryGetFieldValue(parsed, space, "Zone Name", out string zoneName) &&
                        !string.IsNullOrWhiteSpace(zoneName))
                    {
                        zonesWithSpaces.Add(zoneName);
                    }
                }
            }

            if (!referenceListDictionary.TryGetValue("SpaceNames", out HashSet<string> spaceNames))
            {
                spaceNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                referenceListDictionary["SpaceNames"] = spaceNames;
            }

            foreach (string zoneName in zoneNames)
            {
                if (!zonesWithSpaces.Contains(zoneName))
                {
                    spaceNames.Add(zoneName);
                }
            }
        }

    }

    public class ReferenceListResult
    {
        public Dictionary<string, HashSet<string>> ReferenceList;
        public List<IdfError> Errors;

        public ReferenceListResult(Dictionary<string, HashSet<string>> referenceList, List<IdfError> errors)
        {
            ReferenceList = referenceList;
            Errors = errors;
        }
    }
}
