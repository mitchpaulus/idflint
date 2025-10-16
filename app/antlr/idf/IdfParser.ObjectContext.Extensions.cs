using System;
using dotnet;

namespace Idf
{
    public partial class IdfParser
    {
        public partial class ObjectContext
        {
            public bool TryGetFieldValue(IdfObject idfObject, string fieldName, out string value)
            {
                if (idfObject == null) throw new ArgumentNullException(nameof(idfObject));
                return idfObject.TryGetFieldValue(this, fieldName, out value);
            }
        }
    }
}
