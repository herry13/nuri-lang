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
         | TSchema t_object
         | TRef t_object
         | TForward of reference * t_forward

t_object  = TObject
         | TRoot_object
         | TUserSchema of string * t_object

t_forward = TLinkForward
         | TRefForward
