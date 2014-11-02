t        = TBool
         | TInt
         | TFloat
         | TString
         | TNull
         | TEnum of string * string list
         | TUndefined
         | TAny
         | TAction
         | TConstraint

         | TList t
         | TSchema tSchema
         | TRef tSchema
         | TForward of reference * tForward

tSchema  = TObject
         | TRootSchema
         | TUserSchema of string * tSchema

tForward = TLinkForward
         | TRefForward
