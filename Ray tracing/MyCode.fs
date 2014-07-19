module MyCode

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


type Sphere = { center:Point3D; radius:float; diffuseColor:Color }
type Light = { position:Point3D; color:Color }
type Camera = { position:Point3D; lookAt:Point3D; lookUp:Vector3D }
type Scene = { camera:Camera; sphere:Sphere; ambientLight:Color; light:Light }
type Ray = { origin:Point3D; direction:Vector3D }
type Intersection = { normal:Vector3D; point:Point3D; ray:Ray; sphere:Sphere; t:float }

let norm (v:Vector3D) =
    let abs = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
    v / abs

let pointAtTime ray time =
    ray.origin + time * ray.direction

let castRay ray (scene : Scene) = 
    let s = ray.origin - scene.sphere.center
    let rayDir = norm ray.direction
    let sv = Vector3D.DotProduct(s, rayDir)
    let ss = Vector3D.DotProduct(s,s)
    let discr = sv*sv - ss + scene.sphere.radius * scene.sphere.radius
    if discr < 0.0 then []
    else 
        let normalAtTime t = norm (scene.sphere.center - pointAtTime ray t)
        let (t1,t2) = (-sv + sqrt(discr), -sv - sqrt(discr))
        [ { normal = normalAtTime t1; point = pointAtTime ray t1; ray = ray; sphere=scene.sphere; t = t1 };
          { normal = normalAtTime t2; point = pointAtTime ray t2; ray = ray; sphere=scene.sphere; t = t2 } ]

let colorAt intersections scene =
    let closest = List.maxBy(fun i -> i.t) intersections
    let kd = closest.sphere.diffuseColor
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

    // GUI
    let mainForm = new Form(Width = width, Height = height, Text = "FRay")
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    mainForm.Controls.Add(box)
    let bmp = new Bitmap(width, height)

    // Sphere
    let sphere = { center = Point3D(1.0, 1.0, 1.0); radius = 0.4; diffuseColor = Color(1.0, 0.1, 0.1) }

    // Camera
    let camera = { position = Point3D(0.0, 0.0, 0.0); lookAt = Point3D(1.0, 1.0, 1.0); lookUp = Vector3D(0.0, 1.0, 0.0) }

    // Scene
    let light = { position=Point3D(0.0,0.0,-5.0); color=Color(0.8,0.8,0.8) }
    let scene = { camera = camera; sphere = sphere; ambientLight = Color(0.2, 0.2, 0.2); light = light }

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
            let intersects = castRay ray scene
            match intersects with
            | [] -> bmp.SetPixel(x, y, Color.Gray)
            | _ -> let color = colorAt intersects scene
                   bmp.SetPixel(x,y, Color.FromArgb(255, (int)(color.r*255.0), (int)(color.g*255.0), (int)(color.b*255.0)))

    bmp.Save(@"c:\users\mfh\desktop\output1.jpg")

    Application.Run(mainForm)