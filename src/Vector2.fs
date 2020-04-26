namespace FableBounce

open Fable.Core

type Vector2 =
  { X: float
    Y: float }

  static member (+) (v1, v2) =
    { X = v1.X + v2.X
      Y = v1.Y + v2.Y }

  static member (-) (v1, v2) =
    { X = v1.X - v2.X
      Y = v1.Y - v2.Y }

  static member (*) (v, f) =
    { X = v.X * f
      Y = v.Y * f }

  static member (/) (v, f) =
    match f with
    | 0. -> failwith "Cannot divide by 0"
    | _ ->
        { X = v.X / f
          Y = v.Y / f }

module Vector2 =
  let min vValue vMin =
    { X = min vValue.X vMin.X
      Y = min vValue.Y vMin.Y }

  let max vValue vMax =
    { X = max vValue.X vMax.X
      Y = max vValue.Y vMax.Y }

  let flip v =
    { v with
        X = -v.X
        Y = -v.Y }

  let flipX v = { v with X = -v.X }
  let flipY v = { v with Y = -v.Y }
