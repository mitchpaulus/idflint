using System;
using System.IO;
using System.Linq;
using System.Net.Http;

namespace dotnet
{
    /// <summary>
    /// Locates (and downloads on first use) the per-version EnergyPlus object data
    /// published as SQLite files by the mitchpaulus/idf-default-objects project.
    /// Files are cached under ~/.local/share/idf-lint (XDG_DATA_HOME) or the
    /// Windows local application data folder.
    /// </summary>
    public static class IdfDataStore
    {
        // Release tag is pinned so the downloaded schema is reproducible for a given idflint build.
        private const string ReleaseTag = "v0.1.0";
        private const string DownloadUrlFormat = "https://github.com/mitchpaulus/idf-default-objects/releases/download/" + ReleaseTag + "/{0}.sqlite3";

        // Versions published in the pinned release, ascending.
        public static readonly string[] KnownVersions =
        {
            "9.0.1", "9.1.0", "9.2.0", "9.3.0", "9.4.0", "9.5.0", "9.6.0",
            "22.1.0", "22.2.0", "23.1.0", "23.2.0", "24.1.0", "24.2.0",
            "25.1.0", "25.2.0", "26.1.0",
        };

        // Used when the file has no Version object; matches the previously baked-in data.
        public const string DefaultVersion = "24.2.0";

        public static string DataDirectory()
        {
            string overrideDir = Environment.GetEnvironmentVariable("IDF_LINT_DATA_DIR");
            if (!string.IsNullOrWhiteSpace(overrideDir)) return overrideDir;

            // On Linux/macOS this resolves to XDG_DATA_HOME (~/.local/share); on Windows, %LocalAppData%.
            string baseDir = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
            return Path.Combine(baseDir, "idf-lint");
        }

        /// <summary>
        /// Maps a Version object value ("24.2", "24.2.0", "9.4") to the best available
        /// published version. Falls back to the nearest version at or below the request,
        /// or the oldest available if the request predates them all.
        /// </summary>
        public static string ResolveVersion(string requested, out bool exact)
        {
            if (string.IsNullOrWhiteSpace(requested))
            {
                exact = false;
                return DefaultVersion;
            }

            requested = requested.Trim();

            if (KnownVersions.Contains(requested))
            {
                exact = true;
                return requested;
            }

            string prefixMatch = KnownVersions.FirstOrDefault(v => v.StartsWith(requested + ".", StringComparison.Ordinal));
            if (prefixMatch != null)
            {
                exact = true;
                return prefixMatch;
            }

            exact = false;
            if (!TryParseVersion(requested, out int major, out int minor))
            {
                return DefaultVersion;
            }

            string best = null;
            foreach (string candidate in KnownVersions)
            {
                TryParseVersion(candidate, out int cMajor, out int cMinor);
                if (cMajor < major || (cMajor == major && cMinor <= minor)) best = candidate;
            }

            return best ?? KnownVersions[0];
        }

        private static bool TryParseVersion(string text, out int major, out int minor)
        {
            major = 0;
            minor = 0;
            string[] parts = text.Split('.');
            if (parts.Length < 1 || !int.TryParse(parts[0], out major)) return false;
            if (parts.Length > 1) int.TryParse(parts[1], out minor);
            return true;
        }

        /// <summary>
        /// Returns the local path to the SQLite file for a resolved version,
        /// downloading it on first use.
        /// </summary>
        public static string GetDatabasePath(string version)
        {
            string dir = DataDirectory();
            string path = Path.Combine(dir, $"{version}.sqlite3");
            if (File.Exists(path)) return path;

            Directory.CreateDirectory(dir);

            string url = string.Format(DownloadUrlFormat, version);
            Console.Error.WriteLine($"idflint: downloading object data for EnergyPlus {version} to {path}");

            using HttpClient client = new HttpClient();
            byte[] bytes;
            try
            {
                bytes = client.GetByteArrayAsync(url).GetAwaiter().GetResult();
            }
            catch (Exception e) when (e is HttpRequestException || e is System.Threading.Tasks.TaskCanceledException)
            {
                throw new IdfDataStoreException(
                    $"Unable to download object data for EnergyPlus {version} from {url}. " +
                    $"Check your network connection, or manually place the file at {path}.", e);
            }

            // Write to a temp file and move into place so a partial download never
            // masquerades as a valid database.
            string tempPath = path + "." + Guid.NewGuid().ToString("N") + ".tmp";
            File.WriteAllBytes(tempPath, bytes);
            File.Move(tempPath, path, overwrite: true);

            return path;
        }
    }

    public class IdfDataStoreException : Exception
    {
        public IdfDataStoreException(string message, Exception inner) : base(message, inner) { }
    }
}
