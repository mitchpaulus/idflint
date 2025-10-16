
        public static IEnumerable<IdfObject> GetRequiredObjects()
        {
            foreach (var key in Objects.Keys)
            {
                var idfObject = GetIdfObject(key);
                if (idfObject.Required) yield return idfObject;
            }
        }
