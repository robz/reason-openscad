$fn = 100;
union() {
  cube([1., 1., 3.], false);
  translate([1., 0., 0.]) {
    sphere(2.);
  };
};
