namespace FableBounce

open Browser.Types
open Fable.Core.JsInterop

type Rectangle = Vector2 * Vector2

module Rectangle =
  let top (vec1, vec2) = min vec1.Y vec2.Y
  let right (vec1, vec2) = max vec1.X vec2.X
  let bottom (vec1, vec2) = max vec1.Y vec2.Y
  let left (vec1, vec2) = min vec1.X vec2.X

  let width (vec1, vec2) = vec1.X - vec2.X |> abs
  let height (vec1, vec2) = vec1.Y - vec2.Y |> abs

  let center (vec1: Vector2, vec2: Vector2) = (vec1 + vec2) * 0.5

  let topLeft rect =
    { X = left rect
      Y = top rect }

  let topRight rect =
    { X = right rect
      Y = top rect }

  let bottomLeft rect =
    { X = left rect
      Y = bottom rect }

  let bottomRight rect =
    { X = right rect
      Y = bottom rect }

  let containsPoint rect point =
    point.X <= right rect && point.X >= left rect &&

    point.Y <= top rect && point.Y >= bottom rect

  let containsRect container rect =
    right rect <= right container && left rect >= left container &&

    top rect >= top container && bottom rect <= bottom container

  let fromTLWH (top, left, width, height) =
    ({ X = left
       Y = top },
     { X = left + width
       Y = top + height })

  let toLTWH rect = (left rect, top rect, width rect, height rect)

  let toScreenSpace (left, top, width, height) = (left, Config.Height - top, width, -height)

  let draw (ctx: CanvasRenderingContext2D) rect (colour: string) =
    ctx.beginPath()
    ctx.lineWidth <- 0.5
    ctx.strokeStyle <- !^colour
    ctx.strokeRect
      (rect
       |> toLTWH
       |> toScreenSpace)
    ctx.stroke()
