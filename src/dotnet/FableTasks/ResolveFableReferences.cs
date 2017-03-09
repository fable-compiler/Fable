using System;
using System.IO;
using System.Linq;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace Fable
{
    public class ResolveFableReferences : Task
    {
        [Output]
        public string[] Result { get; set; }
        [Required]
        public string[] Input { get; set; }
        [Required]
        public string ProjectDirectory { get; set; }

        string findProject(string directory, string projName) {
            var candidates = Directory.GetFiles(directory, projName);
            if (candidates.Length > 0) {
                return candidates[0];
            }
            throw new Exception("Couldn't find " + projName + " in " + directory);
        }

        string checkDirectory(string directory, string modName, string projName) {
            var testDir = Path.Combine(directory, modName);
            if (Directory.Exists(testDir)) {
                return findProject(testDir, projName);
            }

            testDir = Path.Combine(directory, "node_modules");
            if (Directory.Exists(testDir)) {
                testDir = Path.Combine(testDir, modName);
                if (Directory.Exists(testDir)) {
                    return findProject(testDir, projName);
                }
                throw new Exception("Cannot find " + modName + " in node_modules. Make sure it's in npm package.json.");
            }

            var parent = Path.GetDirectoryName(directory);
            if (parent != null) {
                return checkDirectory(parent, modName, projName);
            }

            throw new Exception("Cannot find module " + modName);
        }

        string resolvePath(string moduleName) {
            var projName = moduleName.Replace("-", ".") + ".fsproj";
            return checkDirectory(this.ProjectDirectory, moduleName, projName);
        }

        public override bool Execute() {
            this.Result = this.Input?.Select(resolvePath).ToArray();
            return true;
        }
    }
}
