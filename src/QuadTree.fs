namespace FableBounce

open Browser.Types
open Fable.Core.JsInterop
open Fable.Core.JS
open Config

type ChildNodes<'T> =
  { NW: QuadTree<'T>
    NE: QuadTree<'T>
    SW: QuadTree<'T>
    SE: QuadTree<'T> }

and LeafNode<'T> =
  { Bounds: Rectangle
    Items: 'T list
    Depth: int }

and ParentNode<'T> =
  { Bounds: Rectangle
    Items: 'T list
    ChildNodes: ChildNodes<'T>
    Depth: int }

and QuadTree<'T> =
  | Leaf of LeafNode<'T>
  | Parent of ParentNode<'T>

module QuadTree =
  [<Literal>]
  let MaxNodeDepth = 6

  [<Literal>]
  let MaxItemsPerNode = 5

  let createEmptyRoot bound =
    { Bounds = bound
      Items = []
      Depth = 0 }
    |> Leaf

  let createLeafNode bound items depth =
    { Bounds = bound
      Items = items
      Depth = depth }
    |> Leaf

  let isRectWithinNode node rect =
    node
    |> function
    | Leaf node -> Rectangle.containsRect node.Bounds rect
    | Parent node -> Rectangle.containsRect node.Bounds rect

  let (|NW|NE|SW|SE|Outside|) (childNodes, rect) =
    let ap =
      if isRectWithinNode childNodes.NW rect then NW
      elif isRectWithinNode childNodes.NE rect then NE
      elif isRectWithinNode childNodes.SW rect then SW
      elif isRectWithinNode childNodes.SE rect then SE
      else Outside
    ap

  let rec insertIntoQuadTree item itemToRectFn quadTree =
    let subdivideLeafNode (node: LeafNode<_>) =
      let center = node.Bounds |> Rectangle.center

      let childNodes =
        { NW = createLeafNode (Rectangle.topLeft node.Bounds, center) [] (node.Depth + 1)
          NE = createLeafNode (center, Rectangle.topRight node.Bounds) [] (node.Depth + 1)
          SE = createLeafNode (center, Rectangle.bottomRight node.Bounds) [] (node.Depth + 1)
          SW = createLeafNode (Rectangle.bottomLeft node.Bounds, center) [] (node.Depth + 1) }

      let parentNode =
        { Bounds = node.Bounds
          Items = []
          ChildNodes = childNodes
          Depth = node.Depth }
        |> Parent

      node.Items
      |> List.fold (fun node item -> insertIntoQuadTree item itemToRectFn node) parentNode

    let insertItemIntoLeafNode item (node: LeafNode<_>) =
      match node.Items.Length with
      | x when x < MaxItemsPerNode -> { node with Items = node.Items @ [ item ] } |> Leaf
      | _ -> subdivideLeafNode node |> insertIntoQuadTree item itemToRectFn

    let insertItemIntoParentNode item node =
      let _nodeWithChildNodes node childNodes = { node with ChildNodes = childNodes }

      match (node.ChildNodes, item |> itemToRectFn) with
      | NW ->
          { node.ChildNodes with NW = insertIntoQuadTree item itemToRectFn node.ChildNodes.NW }
          |> _nodeWithChildNodes node
      | NE ->
          { node.ChildNodes with NE = insertIntoQuadTree item itemToRectFn node.ChildNodes.NE }
          |> _nodeWithChildNodes node
      | SE ->
          { node.ChildNodes with SE = insertIntoQuadTree item itemToRectFn node.ChildNodes.SE }
          |> _nodeWithChildNodes node
      | SW ->
          { node.ChildNodes with SW = insertIntoQuadTree item itemToRectFn node.ChildNodes.SW }
          |> _nodeWithChildNodes node
      | Outside -> { node with Items = node.Items @ [ item ] }
      |> Parent

    match quadTree with
    | Leaf leafNode -> insertItemIntoLeafNode item leafNode
    | Parent parentNode -> insertItemIntoParentNode item parentNode

  let rec countTotalNodes quadTree acc =
    match quadTree with
    | Leaf _ -> acc + 1
    | Parent node ->
        countTotalNodes node.ChildNodes.NW (acc) + countTotalNodes node.ChildNodes.NE (acc)
        + countTotalNodes node.ChildNodes.SE (acc) + countTotalNodes node.ChildNodes.SW (acc)

  let build items itemToRectFn bounds =
    let root = createEmptyRoot bounds
    items |> List.fold (fun state item -> insertIntoQuadTree item itemToRectFn state) root

  let transformToScreenSpace vec =
    vec - { X = 0.
            Y = Height }
    |> Vector2.flipY

  let rec draw (ctx: CanvasRenderingContext2D) quadTree =
    ctx.beginPath()
    ctx.lineWidth <- 1.
    match quadTree with
    | Leaf node -> Rectangle.draw ctx node.Bounds "red"
    | Parent node ->
        node.ChildNodes.NW |> draw ctx
        node.ChildNodes.NE |> draw ctx
        node.ChildNodes.SE |> draw ctx
        node.ChildNodes.SW |> draw ctx
