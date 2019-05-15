using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using Microsoft.Office.Tools.Excel;
using Microsoft.Office.Tools;
using System.IO;
using System.IO.Compression;
using System.Net;

namespace Synthlog
{
    public partial class ThisAddIn
    {
        Home home;
        public Home Home
        {
            get => home;    
        }

        CustomTaskPane taskPane;
        public CustomTaskPane TaskPane
        {
            get => taskPane;
        }

        string synthlog_path;
        public string SynthlogPath
        {
            get => synthlog_path;
        }

        private void ThisAddIn_Startup(object sender, System.EventArgs e)
        {
            home = new Home();
            taskPane = this.CustomTaskPanes.Add(home, "Home");
            taskPane.VisibleChanged += new EventHandler(taskPane_VisibleChanged);
            taskPane.VisibleChanged += new EventHandler(home.OnVisibleChanged);

            synthlog_path = 
                Environment.GetFolderPath(
                    Environment.SpecialFolder.UserProfile
                    ) + "\\.SynthLogAddIn";
            CreateStructure();
        }

        private void ThisAddIn_Shutdown(object sender, System.EventArgs e)
        {
        }

        private void taskPane_VisibleChanged(object sender, System.EventArgs e)
        {
            Globals.Ribbons.Ribbon1.toggleButton1.Checked = taskPane.Visible;
        }

        private void CreateStructure()
        {
            Directory.CreateDirectory(synthlog_path);
            string problog_path = synthlog_path + "\\problog";
            if (!Directory.Exists(problog_path))
                DownloadProblog(synthlog_path);

            string synthlog_library_path = synthlog_path + "\\synthlog";
            if (!Directory.Exists(synthlog_library_path))
                ImportSynthlog(synthlog_path);

            string builtin_path = synthlog_path + "\\builtin";
            Directory.CreateDirectory(builtin_path);
            string init_path = builtin_path + "\\init.pl";
            if (!File.Exists(init_path))
            {
                File.WriteAllText(init_path, Properties.Resources.init);
            }
        }

        private void DownloadProblog(string path)
        {
            string zip_path = path + "\\develop.tmp";

            ServicePointManager.Expect100Continue = true;
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12;

            using (var client = new WebClient())
            {
                client.DownloadFile(
                    "https://bitbucket.org/problog/problog/get/develop.zip",
                    zip_path);
            }

            ZipFile.ExtractToDirectory(zip_path, path);
            string[] dirs = Directory.GetDirectories(path);
            foreach (var dir in dirs)
                if (dir.Split('\\').Last().StartsWith("problog-"))
                    Directory.Move(dir, path + "\\problog");

            File.Delete(zip_path);
        }

        private void ImportSynthlog(string path)
        {
            string zip_path = path + "\\synthlog.zip";
            File.WriteAllBytes(zip_path, Properties.Resources.synthlog);
            ZipFile.ExtractToDirectory(zip_path, path + "\\synthlog");
            File.Delete(zip_path);
        }

        #region Code généré par VSTO

        /// <summary>
        /// Méthode requise pour la prise en charge du concepteur - ne modifiez pas
        /// le contenu de cette méthode avec l'éditeur de code.
        /// </summary>
        private void InternalStartup()
        {
            this.Startup += new System.EventHandler(ThisAddIn_Startup);
            this.Shutdown += new System.EventHandler(ThisAddIn_Shutdown);
        }
        
        #endregion
    }
}
