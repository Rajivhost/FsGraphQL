namespace FsGraphQL.Ast

type Document =
    { Definitions : Definition list }

and Definition =
    | ExecutableDefinition of ExecutableDefinition
    | TypeSystemDefinition of TypeSystemDefinition
    | TypeSystemExtension of TypeSystemExtension

and TypeSystemDefinition =
    | SchemaDefinition of SchemaDefinition
    | TypeDefinition of TypeDefinition
    | DirectiveDefinition of DirectiveDefinition

and DirectiveDefinition =
    { Description : string
      Name : string
      Arguments : InputValueDefinition list
      DirectiveLocations : DirectiveLocation list }

and DirectiveLocation =
    | ExecutableDirectiveLocation of ExecutableDirectiveLocation
    | TypeSystemDirectiveLocation of TypeSystemDirectiveLocation

and ExecutableDirectiveLocation =
    | Query
    | Mutation
    | Subscription
    | Field
    | FragmentDefinition
    | FragmentSpread
    | InlineFragment

and TypeSystemDirectiveLocation =
    | Schema
    | Scalar
    | Object
    | FieldDefinition
    | ArgumentDefinition
    | Interface
    | Union
    | Enum
    | EnumValue
    | InputObject
    | InputFieldDefinition

and TypeSystemExtension =
    | SchemaExtension of SchemaExtension
    | TypeExtension of TypeExtension

and SchemaExtension =
    { Directives : Directive list
      Operations : OperationTypeDefinition list }

and TypeExtension =
    | ScalarTypeExtension of ScalarTypeExtension
    | ObjectTypeExtension of ObjectTypeExtension
    | InterfaceTypeExtension of InterfaceTypeExtension
    | UnionTypeExtension of UnionTypeExtension
    | EnumTypeExtension of EnumTypeExtension
    | InputObjectTypeExtension of InputObjectTypeExtension
    member x.Name =
        match x with
        | ScalarTypeExtension x -> x.Name
        | ObjectTypeExtension x -> x.Name
        | InterfaceTypeExtension x -> x.Name
        | UnionTypeExtension x -> x.Name
        | EnumTypeExtension x -> x.Name
        | InputObjectTypeExtension x -> x.Name
    member x.Directives =
        match x with
        | ScalarTypeExtension x -> x.Directives
        | ObjectTypeExtension x -> x.Directives
        | InterfaceTypeExtension x -> x.Directives
        | UnionTypeExtension x -> x.Directives
        | EnumTypeExtension x -> x.Directives
        | InputObjectTypeExtension x -> x.Directives

and ScalarTypeExtension =
    { Name : string
      Directives : Directive list }

and ObjectTypeExtension =
    { Name : string
      Interfaces : string list
      Directives : Directive list
      Fields : FieldDefinition list }

and InterfaceTypeExtension =
    { Name : string
      Directives : Directive list
      Fields : FieldDefinition list }

and UnionTypeExtension =
    { Name : string
      Directives : Directive list
      UnionMemberTypes : string list }

and EnumTypeExtension =
    { Name : string
      Directives : Directive list
      EnumValues : EnumValueDefinition list }

and InputObjectTypeExtension =
    { Name : string
      Directives : Directive list
      InputFields : InputValueDefinition list }

and TypeDefinition =
    | ScalarTypeDefinition of ScalarTypeDefinition
    | ObjectTypeDefinition of ObjectTypeDefinition
    | InterfaceTypeDefinition of InterfaceTypeDefinition
    | UnionTypeDefinition of UnionTypeDefinition
    | EnumTypeDefinition of EnumTypeDefinition
    | InputObjectTypeDefinition of InputObjectTypeDefinition
    member x.Name =
        match x with
        | ScalarTypeDefinition x -> x.Name
        | ObjectTypeDefinition x -> x.Name
        | InterfaceTypeDefinition x -> x.Name
        | UnionTypeDefinition x -> x.Name
        | EnumTypeDefinition x -> x.Name
        | InputObjectTypeDefinition x -> x.Name
    member x.Directives =
        match x with
        | ScalarTypeDefinition x -> x.Directives
        | ObjectTypeDefinition x -> x.Directives
        | InterfaceTypeDefinition x -> x.Directives
        | UnionTypeDefinition x -> x.Directives
        | EnumTypeDefinition x -> x.Directives
        | InputObjectTypeDefinition x -> x.Directives
    member x.Description =
        match x with
        | ScalarTypeDefinition x -> x.Description
        | ObjectTypeDefinition x -> x.Description
        | InterfaceTypeDefinition x -> x.Description
        | UnionTypeDefinition x -> x.Description
        | EnumTypeDefinition x -> x.Description
        | InputObjectTypeDefinition x -> x.Description

and ScalarTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list }

and ObjectTypeDefinition =
    { Description : string
      Name : string
      Interfaces : string list
      Directives : Directive list
      Fields : FieldDefinition list }

and InterfaceTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list
      Fields : FieldDefinition list }

and UnionTypeDefinition =
    { Description : string
      Name : string 
      Directives : Directive list
      UnionMemberTypes : string list }

and EnumTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list
      EnumValues : EnumValueDefinition list }

and EnumValueDefinition =
    { Description : string
      EnumValue : string
      Directives : Directive list }

and InputObjectTypeDefinition =
    { Description : string
      Name : string
      Directives : Directive list
      InputFields : InputValueDefinition list }

and FieldDefinition =
    { Description : string
      Name : string
      Arguments : InputValueDefinition list
      Type : Type
      Directives : Directive list }

and InputValueDefinition =
    { Description : string
      Name : string
      Type : Type
      DefaultValue : Value
      Directives : Directive list }

and SchemaDefinition =
    { Directives : Directive list
      Operations : OperationTypeDefinition list }

and OperationTypeDefinition =
    { OperationType : OperationType
      Name : string }

and ExecutableDefinition =
    | OperationDefinition of OperationDefinition
    | FragmentDefinition of FragmentDefinition

and FragmentDefinition =
    { Name : string
      TypeCondition : string
      Directives : Directive list 
      SelectionSet : Selection list }
    
and OperationDefinition =
    | QueryShorthand of Selection list
    | Operation of Operation

and Operation =
    { OperationType : OperationType
      Name : string option
      VariableDefinitions : VariableDefinition list
      Directives : Directive list
      SelectionSet : Selection list }

and OperationType =
    | Query
    | Mutation
    | Subscription

and Directive =
    { Name : string
      Arguments : Argument list }

and Argument =
    { Name : string
      Value : Value }

and ObjectField =
    { Name : string
      Value : Value }

and Value =
    | Variable of variableName : string
    | IntValue of int64
    | FloatValue of float
    | StringValue of string
    | BooleanValue of bool
    | NullValue
    | EnumValue of string
    | ListValue of Value list
    | ObjectValue of ObjectField list

and VariableDefinition =
    { Name : string
      Type : Type
      DefaultValue : Value option }

and Type =
    | NamedType of name : string
    | ListType of Type
    | NonNullType of Type

and Selection =
    | Field of Field
    | FragmentSpread of FragmentSpread
    | InlineFragment of InlineFragment

and FragmentSpread =
    { Name : string
      Directives : Directive list }

and InlineFragment =
    { TypeCondition : string option
      Directives : Directive list
      SelectionSet : Selection list }

and Field =
    { Alias : string option
      Name : string
      Arguments : Argument list
      Directives : Directive list
      SelectionSet : Selection list }
    member x.AliasOrName =
        Option.defaultValue x.Name x.Alias