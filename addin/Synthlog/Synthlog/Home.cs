using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.IO;

namespace Synthlog
{
    public partial class Home : UserControl
    {
        public Home()
        {
            InitializeComponent();
        }

        private void db_selection_Click(object sender, EventArgs e)
        {
            openFileDialog1.ShowDialog();
            Textbox_Update();
        }

        private void Home_Load(object sender, EventArgs e)
        {

        }

        private void Textbox_Update()
        {
            db_path.Text = openFileDialog1.FileName;
            if (File.Exists(db_path.Text))
                db_init.Invalidate();
        }

        public void OnVisibleChanged(Object sender, EventArgs e)
        {
            if (this.Visible && Globals.ThisAddIn.Application.ActiveWorkbook != null)
            {
                openFileDialog1.FileName = Globals.ThisAddIn.SynthlogPath + "\\" +
                    Globals.ThisAddIn.Application.ActiveWorkbook.Name.Split('.')[0];
                Textbox_Update();
            }
        }

        private void db_init_Click(object sender, EventArgs e)
        {
            var pw = new ProblogWriter();
            pw.AddFact("excel", "idb", new string[1]{"'"+db_path.Text+"'"});
            pw.AddFact("excel", "workbook_path", new string[1] {
                "'"+Globals.ThisAddIn.Application.ActiveWorkbook.FullName+"'"
            });
            pw.WriteModel(Globals.ThisAddIn.SynthlogPath + "\\parameters.pl");
            var sm = new SynthlogManager();

            MessageBox.Show(sm.Run(Globals.ThisAddIn.SynthlogPath + "\\builtin\\init.pl"));
        }
    }
}