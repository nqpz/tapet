import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/lys/lys"
import "hsl"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge
module dist_int = uniform_int_distribution i32 rnge
type rng = rnge.rng

type point = {y: f32, x: f32, yd: f32, xd: f32,
              p: f32, pd: f32, pf: f32,
              h: f32, s: f32, l: f32, hd: f32}

let mk_n_points (rng: rng): (rng, i32) =
  dist_int.rand (10, 40) rng

let mk_point (rng: rng): point =
  let (rng, y) = dist.rand (0, 1) rng
  let (rng, x) = dist.rand (0, 1) rng
  let (rng, yd) = dist.rand (-0.1, 0.1) rng
  let (rng, xd) = dist.rand (-0.1, 0.1) rng
  let (rng, p) = dist.rand (10, 80) rng
  let (rng, pd) = dist.rand (-1, 1) rng
  let (rng, pf) = dist.rand (1, 10) rng
  let (rng, h) = dist.rand (0, 1) rng
  let (rng, s) = dist.rand (0.25, 0.75) rng
  let (rng, l) = dist.rand (0.25, 0.75) rng
  let (_rng, hd) = dist.rand (-0.1, 0.1) rng
  in {y, x, yd, xd, p, pd, pf, h, s, l, hd}

type text_content = (i32, i32, i32)
module lys: lys with text_content = text_content = {
  type state = {time: f32, paused: bool, h: i32, w: i32,
                n_points: i32, rng: rng, tiles: i32}

  let grab_mouse = false

  let init (seed: u32) (h: i32) (w: i32): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let (rng, n_points) = mk_n_points rng
    in {time=0, paused=false, w, h, n_points, rng, tiles=1}

  let resize (h: i32) (w: i32) (s: state): state =
    s with h = h with w = w

  let keydown (key: i32) (s: state): state =
    if key == SDLK_SPACE
    then s with paused = !s.paused
    else if key == SDLK_r
    then let (rng, n_points) = mk_n_points s.rng
         in s with n_points = n_points with rng = rng
    else if key == SDLK_UP
    then s with tiles = s.tiles + 1
    else if key == SDLK_DOWN
    then s with tiles = i32.max 1 (s.tiles - 1)
    else if key == SDLK_RIGHT
    then s with n_points = s.n_points + 1
    else if key == SDLK_LEFT
    then s with n_points = i32.max 1 (s.n_points - 1)
    else s

  let event (e: event) (s: state): state =
    match e
    case #step td ->
      s with time = s.time + if s.paused then 0 else td
    case #keydown {key} ->
      keydown key s
    case _ -> s

  let dist_sq ((y1, x1): (f32, f32)) ((y2, x2): (f32, f32)): f32 =
    let d t2 t1 =
      let t = f32.abs (t2 - t1) % 1
      in f32.min t (1 - t)
    in d y2 y1 ** 2 + d x2 x1 ** 2

  let render (s: state): [][]i32 =
    let rngs = rnge.split_rng s.n_points s.rng
    let points = map mk_point rngs
    let ts = i32.min (s.h / s.tiles) (s.w / s.tiles)
    let render_pixel (y: i32) (x: i32): argb.colour =
      let coor1 = (r32 y / r32 ts, r32 x / r32 ts)
      let ds = map (\p ->
                      let coor2 = (p.y + p.yd * s.time, p.x + p.xd * s.time)
                      in 1 / (0.5 + dist_sq coor1 coor2) ** (p.p + p.pf * f32.sin (p.pd * s.time))
                   ) points
      let dsum = reduce_comm (+) 0 ds
      let (r, g, b) = reduce_comm (\(r1, g1, b1) (r2, g2, b2) ->
                                     (r1 + r2, g1 + g2, b1 + b2)) (0, 0, 0)
                                  (map2 (\p d ->
                                           let d' = d / dsum
                                           let (r, g, b) = hsl_to_rgb ((p.h + p.hd * s.time) % 1) p.s p.l
                                           in (d' * r, d' * g, d' * b))
                                       points ds)
      in argb.from_rgba r g b 1
    let pixels = tabulate_2d ts ts render_pixel
    in if s.h == ts && s.w == ts then pixels
       else tabulate_2d s.h s.w (\y x -> pixels[y % ts, x % ts])

  type text_content = text_content

  let text_format () = "Points: %d\nTiles: %dx\nFPS: %d"

  let text_content (render_duration: f32) (s: state): text_content =
    (s.n_points, s.tiles, t32 render_duration)

  let text_colour = const argb.black
}
