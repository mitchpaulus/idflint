using System;
using System.Collections.Generic;
using System.Reflection;

namespace dotnet
{
    public static class IdfRequiredObjects
    {
        private static readonly Lazy<IReadOnlyList<string>> _requiredObjectNames =
            new Lazy<IReadOnlyList<string>>(DiscoverRequiredObjectNames);

        public static IReadOnlyList<string> RequiredObjectNames => _requiredObjectNames.Value;

        private static IReadOnlyList<string> DiscoverRequiredObjectNames()
        {
            const BindingFlags bindingFlags = BindingFlags.Static | BindingFlags.NonPublic;
            var objectsField = typeof(IdfObjectListV242).GetField("Objects", bindingFlags);
            if (objectsField == null)
            {
                throw new InvalidOperationException("Unable to locate the IdfObject definitions cache.");
            }

            if (objectsField.GetValue(null) is not Dictionary<string, Func<IdfObject>> objectFactories)
            {
                throw new InvalidOperationException("The IdfObject definitions cache has an unexpected type.");
            }

            var requiredNames = new List<string>();
            foreach (var objectName in objectFactories.Keys)
            {
                var idfObject = IdfObjectListV242.GetIdfObject(objectName);
                if (idfObject.Required)
                {
                    requiredNames.Add(idfObject.Name);
                }
            }

            return requiredNames;
        }
    }
}
