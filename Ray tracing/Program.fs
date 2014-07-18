﻿
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.Drawing
open System.Windows.Forms
open System.Windows.Media.Media3D

type Color(r: float, g: float, b: float) = 
    member this.r = r
    member this.g = g
    member this.b = b
    static member ( * ) (c1: Color, c2: Color) =
        Color (c1.r * c2.r, c1.g * c2.g, c1.b * c2.b)
    static member ( * ) (c: Color, s: float) = 
        Color (c.r * s, c.g * s, c.b * s)
    static member ( + ) (c1: Color, c2: Color) = 
        let r = Math.Min (c1.r + c2.r, 1.0)
        let g = Math.Min (c1.g + c2.g, 1.0)
        let b = Math.Min (c1.b + c2.b, 1.0)
        Color(r,g,b)
    static member Zero = Color(0.0, 0.0, 0.0)

type Shape =
    | Sphere  of center:Point3D * radius:float * diffuseColor:Color
    | Triangle of a:Point3D * b:Point3D * c:Point3D * v1:Vector3D * v2:Vector3D * v3:Vector3D * diffuseColor:Color
type Light = { position:Point3D; color:Color }
type Camera = { position:Point3D; lookAt:Point3D; lookUp:Vector3D }
type Scene = { camera:Camera; shapes:list<Shape>; ambientLight:Color; light:Light }
type Ray = { origin:Point3D; direction:Vector3D }
type Intersection = { normal:Vector3D; point:Point3D; ray:Ray; shape:Shape; t:float; color:Color }

let norm (v:Vector3D) =
    let abs = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
    v / abs

let cramersDet v1 v2 v3 =
    Vector3D.DotProduct(Vector3D.CrossProduct(v1, v2), v3)

let cramers a b c ray =
    let A1 = a - b
    let A2 = a - c
    let A3 = ray.direction
    let A4 = a - ray.origin
    let detA = cramersDet A1 A2 A3
    let det1 = cramersDet A4 A2 A3
    let det2 = cramersDet A1 A4 A3
    let det3 = cramersDet A1 A2 A4
    let beta = det1 / detA
    let y = det2 /detA
    let t = det3 / detA
    let alpha = 1.0 - beta - y
    (alpha,beta,y,t)

let pointAtTime ray time =
    ray.origin + time * ray.direction

let intersection shape ray = 
    match shape with
        | Sphere (center, radius, diffuseColor) ->
            let s = ray.origin - center
            let rayDir = norm ray.direction
            let sv = Vector3D.DotProduct(s, rayDir)
            let ss = Vector3D.DotProduct(s,s)
            let discr = sv*sv - ss + radius * radius
            if discr < 0.0 then []
            else
                let normalAtTime t = norm ((pointAtTime ray t) - center)
                let (t1,t2) = (-sv + sqrt(discr), -sv - sqrt(discr))
                [ { normal = normalAtTime t1; point = pointAtTime ray t1; ray = ray; shape=shape; t = t1; color=diffuseColor };
                  { normal = normalAtTime t2; point = pointAtTime ray t2; ray = ray; shape=shape; t = t2; color=diffuseColor } ]
        | Triangle(a,b,c,v1,v2,v3,color) ->
            let (alpha,beta,y,t) = cramers a b c ray
            let n = alpha * v1 + beta * v2 + y * v3
            let p = pointAtTime ray t
            [ { normal = n; point = p; ray = ray; shape = shape; t = t; color=color } ]

let colorAt intersections scene =
    let closest = List.minBy(fun i -> i.t) intersections
    let kd = closest.color
    let Ia = scene.ambientLight
    let L = norm (scene.light.position - closest.point)
    let Id = Math.Max(0.0, Vector3D.DotProduct(L, closest.normal))
    let V = scene.camera.position - closest.point
    let H = norm (L + V)
    let Is = Math.Pow(Math.Max(0.0, Vector3D.DotProduct(H,closest.normal)), 500.0)
    let ambient = kd * Ia
    let diffuse = kd * Id
    let specular = Color(1.0,1.0,1.0) * Is
    ambient + diffuse + specular

do
    let width = 480
    let height = 320

    // Vertical and horiontal field of view:
    let hfov = Math.PI/3.5
    let vfov = hfov * float(height) / float(width)

    // Pixel width and height
    let pw = 2.0 * Math.Tan(float(hfov / 2.0)) / float(width)
    let ph = 2.0 * Math.Tan(float(vfov / 2.0)) / float(height)

    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    let bmp = new Bitmap(width, height)

    // Sphere
    let sphere = Sphere(Point3D(1.0, 1.0, 1.0), 0.4, Color(1.0, 0.1, 0.1))

    // Camera
    let camera = { position = Point3D(0.0, 0.0, 0.0); lookAt = Point3D(1.0, 1.0, 1.0); lookUp = Vector3D(0.0, 1.0, 0.0) }

    // Scene
    let light = { position=Point3D(0.0,0.0,-5.0); color=Color(0.8,0.8,0.8) }
    let scene = { camera = camera; shapes = [sphere]; ambientLight = Color(0.2, 0.2, 0.2); light = light }

    // Set up the coordinate system
    let n = norm (camera.position - camera.lookAt)
    let u = norm (Vector3D.CrossProduct(n, camera.lookUp))
    let v = norm (Vector3D.CrossProduct(n, u))
    let vpc = camera.position - n

    for x in 0..(width - 1) do
        for y in 0..(height - 1) do 
            let rayPoint = vpc + float(x-width/2)*pw*u + float(y-height/2)*ph*v
            let rayDir = norm (rayPoint - scene.camera.position)
            let ray = { origin = scene.camera.position; direction = rayDir }
            let intersects = List.collect (fun x -> x) (List.map (fun x -> intersection x ray) scene.shapes)
            match intersects with
            | [] -> bmp.SetPixel(x, y, Color.Gray)
            | _ -> let color = colorAt intersects scene
                   bmp.SetPixel(x,y, Color.FromArgb(255, (int)(color.r*255.0), (int)(color.g*255.0), (int)(color.b*255.0)))

    bmp.Save(@"c:\users\mfh\desktop\output1.jpg")