# Using Reason ML to output OpenSCAD code

OpenSCAD (http://www.openscad.org/) is a small language used to describe 3D objects. Writing OpenSCAD code feels a lot like using JSX in React (https://reactjs.org/); shapes are expressed in a nested way, similar to how you might use JSX to express nested HTML.

Reason (https://reasonml.github.io/) is a syntax for OCaml. Reason surfaces JSX as a language feature, effectively transforming JSX tags in Reason code into function calls with labeled arguments (https://reasonml.github.io/docs/en/jsx).

While playing around with Reason's JSX feature, I made a set of functions that can be called with JSX tags to output OpenSCAD code.

For example, a cube in OpenSCAD is represented like this:

```
cube([1, 2, 3]);
```

With the functions defined in this repo, that would map to this in Reason:

```
<cube size=(1., 1., 3.) />;
```

Likewise, a shape made from combining a cube and a sphere would look like this in OpenSCAD:

```
union() {
  cube([1, 1, 3]);
  translate([1, 0, 0]) {
    sphere(r=2);
  }
}
```

And in Reason:

```
<union>
  <cube size=(1., 1., 3.) />
  <translate by=(1., 0., 0.)> 
    <sphere r=2. /> 
  </translate>
</union>;
```

How to use:
1. Change the shape at the bottom of `main.re`
2. Run `dune build output.scad` to regenerate the `output.scad` file
3. Open `output.scad` in OpenSCAD
