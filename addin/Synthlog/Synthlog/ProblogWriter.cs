using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace Synthlog
{
    public class ProblogWriter
    {
        public class Predicate
        {
            string scope;
            string predicate;
            string[] args;

            public Predicate(string scope, string predicate, string[] args)
            {
                this.scope = scope;
                this.predicate = predicate;
                this.args = args;
            }

            public override string ToString()
            {
                string to_str = scope + ":" + predicate;
                if (args.Length > 0)
                    to_str += "(" + string.Join(",", args) + ")";
                return to_str;
            }
        }

        string problog_model;

        public ProblogWriter()
        {
            problog_model = "";
        }

        public Predicate CreatePredicate(string scope, string predicate, string[] args)
        {
            return new Predicate(scope, predicate, args);
        }

        public void AddFact(string scope, string predicate, string[] args)
        {
            Predicate pred = CreatePredicate(scope, predicate, args);
            problog_model += pred.ToString() + ". ";
        }

        public void WriteModel(string path)
        {
            File.WriteAllText(path, problog_model);
        }
    }
}
