module AngleSharp.Combinators.Html

open System.Collections.Generic
open System.Linq
open AngleSharp.Dom

let children (elem: IElement) =
  elem.Children :> seq<IElement>

let childrenWhere pred =
  children
  >> Seq.filter pred

let hasChildrenWhere pred =
  childrenWhere pred
  >> Seq.isEmpty
  >> not

let rec selfAndSuccWhere recurseOnMatch pred (elem: IElement) = 
  match struct(pred elem,recurseOnMatch) with
  | struct(true,false) -> Seq.singleton elem
  | struct(true,true) ->
    let matchedChildren = 
        Seq.collect (selfAndSuccWhere recurseOnMatch pred) elem.Children
    matchedChildren.Prepend elem
  | struct(false,_) ->
    Seq.collect (selfAndSuccWhere recurseOnMatch pred) elem.Children

let succWhere recurseOnMatch pred =
  children
  >> Seq.collect (selfAndSuccWhere recurseOnMatch pred)

let hasSuccWhere pred = 
  succWhere false pred
  >> Seq.isEmpty
  >> not

let localName (elem: IElement) =
  elem.LocalName

let hasLocalName name =
  localName
  >> ((=)name)

let private inSet (set: HashSet<'a>) a =
  set.Contains a

let private seqToSet (elems: 'a seq) = 
  HashSet elems

let private seqToPred elems =
  seqToSet elems
  |> inSet  

let childrenWithLocalName names =
  childrenWhere (localName >> seqToPred names)

let selfAndSuccWithLocalName recurseOnMatch names =
  selfAndSuccWhere recurseOnMatch (localName >> seqToPred names)

let succWithLocalName recurseOnMatch names =
  succWhere recurseOnMatch (localName >> seqToPred names)

let elemId (elem: IElement) = 
  elem.Id

let hasId id =
  elemId
  >> ((=)id)

let childrenWithId id = 
  childrenWhere (hasId id)

let firstChildWithId id =
  children
  >> Seq.tryFind (hasId id)

let selfAndSuccWithId recurseOnMatch id =
  selfAndSuccWhere recurseOnMatch (hasId id)

let succWithId recurseOnMatch id =
  succWhere recurseOnMatch (hasId id)

let firstWithId id =
  selfAndSuccWithId false id
  >> Seq.tryFind (fun _ -> true)

let hasClass hClass (elem: IElement) =
  elem.ClassList.Contains hClass

let predAnd pred1 pred2 a = 
  pred1 a && pred2 a

let predAnds preds = 
  Seq.fold predAnd (fun _ -> true) preds

let predOr pred1 pred2 a = 
  pred1 a || pred2 a

let predOrs preds =
  Seq.fold predOr (fun _ -> false) preds

let private classesToAndPred classes =
  Seq.map hasClass classes
  |> predAnds

let childrenWithClasses classes =
  childrenWhere (classesToAndPred classes)

let selfAndSuccWithClasses recurseOnMatch classes =
  selfAndSuccWhere recurseOnMatch (classesToAndPred classes)

let succWithcClasses recurseOnMatch classes =
  succWhere recurseOnMatch (classesToAndPred classes)

let attributes (elem: IElement) = 
  elem.Attributes :> seq<IAttr>

let private attrToLocalNameValuePair (attr: IAttr) = 
  struct(attr.LocalName,attr.Value)

let attributePairs elem = 
  attributes elem
  |> Seq.map attrToLocalNameValuePair

let hasAttribute attr (elem: IElement) =
  elem.HasAttribute attr

let getAttribute attr (elem: IElement) =
  Option.ofObj (elem.GetAttribute attr)

let hasAttributeWithValue attr value (elem: IElement) =
  match elem.GetAttribute attr with
  | null -> false
  | a -> a = value

let fromBody (selector: IElement -> 'a) (doc: IDocument) =
  selector doc.Body

let fromHead (selector: IElement -> 'a) (doc: IDocument) =
  selector doc.Head