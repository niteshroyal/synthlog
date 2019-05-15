using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Diagnostics;

namespace Synthlog
{
    public class SynthlogManager
    {
        public string ProblogPath;
        public string SynthlogPath;
        public string PythonPath;

        public SynthlogManager()
        {
            string root = Globals.ThisAddIn.SynthlogPath;

            PythonPath = "python";
            ProblogPath = root+"\\problog\\problog-cli.py";
            SynthlogPath = root+"\\synthlog";

            string path = Environment.GetEnvironmentVariable(
                "PYTHONPATH", EnvironmentVariableTarget.Machine) +
                ";" + SynthlogPath;
            Environment.SetEnvironmentVariable(
                "PYTHONPATH", path, EnvironmentVariableTarget.Process
                );
        }

        public string Run(string file_path)
        {
            if (!File.Exists(file_path))
            {
                // file error
                return "File error";
            }

            string root = Globals.ThisAddIn.SynthlogPath;

            Process process = new Process();
            process.StartInfo.FileName = PythonPath;
            process.StartInfo.Arguments =
                ProblogPath + " " +
                SynthlogPath + "\\synthlog\\environment.pl " +
                file_path + " " +  root + "\\parameters.pl --combine";
            process.StartInfo.UseShellExecute = false;
            process.StartInfo.CreateNoWindow = true;
            process.StartInfo.RedirectStandardError = true;
            process.StartInfo.RedirectStandardOutput = true;
            process.Start();

            string output = process.StandardError.ReadToEnd();
            output += "--- \n" + process.StandardOutput.ReadToEnd();
            process.WaitForExit();

            return output;
        }
    }
}
