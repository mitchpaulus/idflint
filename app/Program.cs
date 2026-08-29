using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace dotnet
{
    class Program
    {
        static int Main(string[] args)
        {
            if (args.Any(arg => arg == "-h" || arg == "--help"))
            {
                Console.WriteLine("idflint\n\nUSAGE:\nidflint idf_file\n\nidflint lints your idf file for great good.\n");
                return 0;
            }

            TextReader reader = args.Any() ? new StreamReader(args[0]) : Console.In;

            IdfLinter linter = new IdfLinter(reader);
            List<IdfError> errors;
            try
            {
                errors = linter.Lint();
            }
            catch (IdfDataStoreException e)
            {
                Console.Error.WriteLine($"idflint: {e.Message}");
                return 2;
            }
            errors.WriteErrors();

            return errors.Any() ? 1 : 0;
        }
    }

}
