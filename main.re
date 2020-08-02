type point2d = (float, float);
type point3d = (float, float, float);
type point4d = (float, float, float, float);
type matrix = (point4d, point4d, point4d, point4d);

let string_of_list = (f, li) =>
  "[" ++ String.concat(", ", List.map(f, li)) ++ "]";
let string_of_floats = string_of_list(string_of_float);
let string_of_point2d = ((x, y)) => string_of_floats([x, y]);
let string_of_point3d = ((x, y, z)) => string_of_floats([x, y, z]);
let string_of_point4d = ((x, y, z, w)) => string_of_floats([x, y, z, w]);
let string_of_matrix = ((x, y, z, w)) =>
  string_of_list(string_of_point4d, [x, y, z, w]);

type param =
  | Bool(bool)
  | Int(int)
  | Float(float)
  | Str(string)
  | Point2d(point2d)
  | Point3d(point3d)
  | Point4d(point4d)
  | Matrix(matrix)
  | Point2dList(list(point2d))
  | Point3dList(list(point3d))
  | IntList(list(int))
  | IntListList(list(list(int)));

let string_of_param =
  fun
  | Bool(b) => string_of_bool(b)
  | Int(i) => string_of_int(i)
  | Float(f) => string_of_float(f)
  | Str(s) => s
  | Point2d(point3d) => string_of_point2d(point3d)
  | Point3d(point3d) => string_of_point3d(point3d)
  | Point4d(point3d) => string_of_point4d(point3d)
  | Matrix(m) => string_of_matrix(m)
  | Point2dList(li) => string_of_list(string_of_point2d, li)
  | Point3dList(li) => string_of_list(string_of_point3d, li)
  | IntList(li) => string_of_list(string_of_int, li)
  | IntListList(li) => string_of_list(string_of_list(string_of_int), li);

let string_of_call = (name, params) =>
  name
  ++ "("
  ++ String.concat(", ", List.map(string_of_param, params))
  ++ ")";

let string_of_call_semi = (name, params) =>
  string_of_call(name, params) ++ ";";

let string_of_call_named = (name, params) =>
  name
  ++ "("
  ++ String.concat(
       ", ",
       List.map(
         ((name, param)) => name ++ "=" ++ string_of_param(param),
         params,
       ),
     )
  ++ ");";

type transform =
  | Translate(point3d)
  | Rotate(point3d)
  | Scale(point3d)
  | Resize(point3d, bool)
  | Mirror(point3d)
  | MultMatrix(matrix)
  | Color_name(string, float)
  | Color_hex(string)
  | Color(point4d)
  | Offset_r(float, bool)
  | Offset_delta(float, bool)
  | Linear_extrude(float, bool, int, int, int)
  | Rotate_extrude(float, int)
  | Projection(bool)
  | Hull
  | Minkowski
  | Union
  | Difference
  | Intersection;

type t =
  | Circle_r(float)
  | Circle_d(float)
  | Square(point2d, bool)
  | Polygon(list(point2d))
  | Polygon_paths(list(point2d), list(int))
  | Text(string, float, string, string, string, float, string, string, string)
  | Sphere_r(float)
  | Sphere_d(float)
  | Cube(point3d, bool)
  | Cylinder_r(float, float, bool)
  | Cylinder_d(float, float, bool)
  | Cylinder_rr(float, float, float, bool)
  | Cylinder_dd(float, float, float, bool)
  | Polyhedron(list(point3d), list(list(int)), int)
  | Import(string)
  | Surface(string, bool, int)
  | Transform(transform, list(t));

exception InvalidParams;

let transform = (trans, ~children, ()) => Transform(trans, children);
let translate = (~by: point3d) => transform(Translate(by));
let rotate = (~by: point3d) => transform(Rotate(by));
let scale = (~by: point3d) => transform(Scale(by));
let resize = (~by: point3d, ~auto: bool) => transform(Resize(by, auto));
let mirror = (~by: point3d) => transform(Mirror(by));
let multMatrix = (~m: matrix) => transform(MultMatrix(m));
let color =
    (
      ~hex: option(string)=?,
      ~colorname: option(string)=?,
      ~alpha: option(float)=?,
      ~color: option(point4d)=?,
    ) =>
  switch (hex, colorname, alpha, color) {
  | (Some(hex), None, None, None) => transform(Color_hex(hex))
  | (None, Some(colorname), Some(alpha), None) =>
    transform(Color_name(colorname, alpha))
  | (None, None, None, Some(color)) => transform(Color(color))
  | _ => raise(InvalidParams)
  };
let offset = (~r: option(float)=?, ~delta: option(float)=?, ~chamfer: bool) =>
  switch (r, delta) {
  | (Some(r), None) => transform(Offset_r(r, chamfer))
  | (None, Some(delta)) => transform(Offset_delta(delta, chamfer))
  | _ => raise(InvalidParams)
  };
let linear_extrude =
    (
      ~height: float,
      ~center: bool=false,
      ~convexity: int,
      ~twist: int,
      ~slices: int,
    ) =>
  transform(Linear_extrude(height, center, convexity, twist, slices));
let rotate_extrude = (~angle: float, ~convexity: int) =>
  transform(Rotate_extrude(angle, convexity));
let projection = (~cut: bool) => transform(Projection(cut));
let hull = transform(Hull);
let minkowski = transform(Minkowski);
let union = transform(Union);
let difference = transform(Difference);
let intersection = transform(Intersection);

let string_of_transform =
  fun
  | Translate(by) => string_of_call("translate", [Point3d(by)])
  | Rotate(by) => string_of_call("rotate", [Point3d(by)])
  | Scale(by) => string_of_call("scale", [Point3d(by)])
  | Resize(by, auto) =>
    string_of_call("resize", [Point3d(by), Bool(auto)])
  | Mirror(by) => string_of_call("mirror", [Point3d(by)])
  | MultMatrix(m) => string_of_call("multMatrix", [Matrix(m)])
  | Color_name(colorname, alpha) =>
    string_of_call("color", [Str(colorname), Float(alpha)])
  | Color_hex(hex) => string_of_call("color", [Str(hex)])
  | Color(color) => string_of_call("color", [Point4d(color)])
  | Offset_r(r, chamfer) =>
    string_of_call("offset_r", [Float(r), Bool(chamfer)])
  | Offset_delta(delta, chamfer) =>
    string_of_call("offset_delta", [Float(delta), Bool(chamfer)])
  | Linear_extrude(height, center, convexity, twist, slices) =>
    string_of_call(
      "linear_extrude",
      [
        Float(height),
        Bool(center),
        Int(convexity),
        Int(twist),
        Int(slices),
      ],
    )
  | Rotate_extrude(angle, convexity) =>
    string_of_call("rotate_extrude", [Float(angle), Int(convexity)])
  | Hull => string_of_call("hull", [])
  | Minkowski => string_of_call("minkowski", [])
  | Projection(cut) => string_of_call("projection", [Bool(cut)])
  | Union => string_of_call("union", [])
  | Difference => string_of_call("difference", [])
  | Intersection => string_of_call("intersection", []);

let shape = (sh, ~children as _, ()) => sh;
let circle = (~r: option(float)=?, ~d: option(float)=?) =>
  switch (r, d) {
  | (None, None)
  | (Some(_), Some(_)) => raise(InvalidParams)
  | (Some(r), None) => shape(Circle_r(r))
  | (None, Some(d)) => shape(Circle_d(d))
  };
let square = (~size: point2d, ~center: bool=false) =>
  shape(Square(size, center));
let polygon = (~points: list(point2d)) => shape(Polygon(points));
let polygon_paths = (~points: list(point2d), ~paths: list(int)) =>
  shape(Polygon_paths(points, paths));
let text =
    (
      ~text: string,
      ~size: float,
      ~font: string,
      ~halign: string,
      ~valign: string,
      ~spacing: float,
      ~direction: string,
      ~language: string,
      ~script: string,
    ) =>
  shape(
    Text(
      text,
      size,
      font,
      halign,
      valign,
      spacing,
      direction,
      language,
      script,
    ),
  );

let sphere = (~r: option(float)=?, ~d: option(float)=?) =>
  switch (r, d) {
  | (None, None)
  | (Some(_), Some(_)) => raise(InvalidParams)
  | (Some(r), None) => shape(Sphere_r(r))
  | (None, Some(d)) => shape(Sphere_d(d))
  };
let cube = (~size: point3d, ~center: bool=false) =>
  shape(Cube(size, center));
let cylinder =
    (
      ~height: float,
      ~r: option(float)=?,
      ~r1: option(float)=?,
      ~r2: option(float)=?,
      ~d: option(float)=?,
      ~d1: option(float)=?,
      ~d2: option(float)=?,
      ~center: bool=false,
    ) =>
  switch (r, r1, r2, d, d1, d2) {
  | (Some(r), None, None, None, None, None) =>
    shape(Cylinder_r(height, r, center))
  | (None, Some(r1), Some(r2), None, None, None) =>
    shape(Cylinder_rr(height, r1, r2, center))
  | (None, None, None, Some(d), None, None) =>
    shape(Cylinder_d(height, d, center))
  | (None, None, None, None, Some(d1), Some(d2)) =>
    shape(Cylinder_dd(height, d1, d2, center))
  | _ => raise(InvalidParams)
  };
let polyhedron =
    (~points: list(point3d), ~faces: list(list(int)), ~convexity: int) =>
  shape(Polyhedron(points, faces, convexity));
let import = (~filename: string) => shape(Import(filename));
let surface = (~filename: string, ~center: bool=false, ~convexity: int) =>
  shape(Surface(filename, center, convexity));

let rec string_of_shape = (tab, shape) =>
  tab
  ++ (
    switch (shape) {
    | Circle_r(r) => string_of_call_semi("circle", [Float(r)])
    | Circle_d(d) => string_of_call_named("circle", [("d", Float(d))])
    | Square(size, center) =>
      string_of_call_semi("square", [Point2d(size), Bool(center)])
    | Polygon(points) =>
      string_of_call_semi("polygon", [Point2dList(points)])
    | Polygon_paths(points, paths) =>
      string_of_call_semi("polygon", [Point2dList(points), IntList(paths)])
    | Text(
        text,
        size,
        font,
        halign,
        valign,
        spacing,
        direction,
        language,
        script,
      ) =>
      string_of_call_semi(
        "text",
        [
          Str(text),
          Float(size),
          Str(font),
          Str(halign),
          Str(valign),
          Float(spacing),
          Str(direction),
          Str(language),
          Str(script),
        ],
      )
    | Sphere_r(r) => string_of_call_semi("sphere", [Float(r)])
    | Sphere_d(d) => string_of_call_named("sphere", [("d", Float(d))])
    | Cube(size, center) =>
      string_of_call_semi("cube", [Point3d(size), Bool(center)])
    | Cylinder_r(height, r, center) =>
      string_of_call_named(
        "cylinder",
        [
          ("height", Float(height)),
          ("r", Float(r)),
          ("center", Bool(center)),
        ],
      )
    | Cylinder_d(height, d, center) =>
      string_of_call_named(
        "cylinder",
        [
          ("height", Float(height)),
          ("d", Float(d)),
          ("center", Bool(center)),
        ],
      )
    | Cylinder_rr(height, r1, r2, center) =>
      string_of_call_named(
        "cylinder",
        [
          ("height", Float(height)),
          ("r1", Float(r1)),
          ("r2", Float(r2)),
          ("center", Bool(center)),
        ],
      )
    | Cylinder_dd(height, d1, d2, center) =>
      string_of_call_named(
        "cylinder",
        [
          ("height", Float(height)),
          ("d1", Float(d1)),
          ("d2", Float(d2)),
          ("center", Bool(center)),
        ],
      )
    | Polyhedron(points, faces, convexity) =>
      string_of_call_semi(
        "polyhedron",
        [Point3dList(points), IntListList(faces), Int(convexity)],
      )
    | Import(filename) => string_of_call_semi("import", [Str(filename)])
    | Surface(filename, center, convexity) =>
      string_of_call_semi(
        "surface",
        [Str(filename), Bool(center), Int(convexity)],
      )
    | Transform(transform, shapes) =>
      string_of_transform(transform)
      ++ " {\n"
      ++ String.concat(
           "\n",
           List.map(string_of_shape(tab ++ "  "), shapes),
         )
      ++ "\n"
      ++ tab
      ++ "};"
    }
  );

print_endline("$fn = 100;");
print_endline(
  string_of_shape(
    "",
    <union>
      <cube size=(1., 1., 3.) />
      <translate by=(1., 0., 0.)> <sphere r=2. /> </translate>
    </union>,
  ),
);
