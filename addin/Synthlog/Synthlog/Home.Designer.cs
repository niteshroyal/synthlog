namespace Synthlog
{
    partial class Home
    {
        /// <summary> 
        /// Variable nécessaire au concepteur.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Nettoyage des ressources utilisées.
        /// </summary>
        /// <param name="disposing">true si les ressources managées doivent être supprimées ; sinon, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Code généré par le Concepteur de composants

        /// <summary> 
        /// Méthode requise pour la prise en charge du concepteur - ne modifiez pas 
        /// le contenu de cette méthode avec l'éditeur de code.
        /// </summary>
        private void InitializeComponent()
        {
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.db_path = new System.Windows.Forms.TextBox();
            this.db_selection = new System.Windows.Forms.Button();
            this.db_init = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.FileName = "openFileDialog1";
            // 
            // db_path
            // 
            this.db_path.Location = new System.Drawing.Point(20, 35);
            this.db_path.Name = "db_path";
            this.db_path.Size = new System.Drawing.Size(100, 20);
            this.db_path.TabIndex = 0;
            // 
            // db_selection
            // 
            this.db_selection.Location = new System.Drawing.Point(126, 35);
            this.db_selection.Name = "db_selection";
            this.db_selection.Size = new System.Drawing.Size(49, 20);
            this.db_selection.TabIndex = 1;
            this.db_selection.Text = "search";
            this.db_selection.UseVisualStyleBackColor = true;
            this.db_selection.Click += new System.EventHandler(this.db_selection_Click);
            // 
            // db_init
            // 
            this.db_init.Location = new System.Drawing.Point(58, 61);
            this.db_init.Name = "db_init";
            this.db_init.Size = new System.Drawing.Size(75, 23);
            this.db_init.TabIndex = 3;
            this.db_init.Text = "Initialize";
            this.db_init.UseVisualStyleBackColor = true;
            this.db_init.Click += new System.EventHandler(this.db_init_Click);
            // 
            // Home
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.db_init);
            this.Controls.Add(this.db_selection);
            this.Controls.Add(this.db_path);
            this.Name = "Home";
            this.Size = new System.Drawing.Size(199, 446);
            this.Load += new System.EventHandler(this.Home_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.Button db_selection;
        private System.Windows.Forms.TextBox db_path;
        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private System.Windows.Forms.Button db_init;
    }
}
