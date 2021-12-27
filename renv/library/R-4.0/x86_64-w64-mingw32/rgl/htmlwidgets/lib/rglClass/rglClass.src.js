//// To generate the help pages for this library, use

// jsdoc --destination ../../../doc/rglwidgetClass --template ~/node_modules/jsdoc-baseline rglClass.src.js

// To validate, set environment variable RGL_DEBUGGING=true
// before building.

/* globals rglwidgetClass: true */

/**
 * The class of an rgl widget
 * @class
*/
rglwidgetClass = function() {
    this.canvas = null;
    this.userMatrix = new CanvasMatrix4();
    this.types = [];
    this.prMatrix = new CanvasMatrix4();
    this.mvMatrix = new CanvasMatrix4();
    this.vp = null;
    this.prmvMatrix = null;
    this.origs = null;
    this.gl = null;
    this.scene = null;
    this.select = {state: "inactive", subscene: null, region: {p1: {x:0, y:0}, p2: {x:0, y:0}}};
    this.drawing = false;
};


    rglwidgetClass.prototype.f_is_lit = 1;
    rglwidgetClass.prototype.f_is_smooth = 2;
    rglwidgetClass.prototype.f_has_texture = 4;
    rglwidgetClass.prototype.f_depth_sort = 8;
    rglwidgetClass.prototype.f_fixed_quads = 16;
    rglwidgetClass.prototype.f_is_transparent = 32;
    rglwidgetClass.prototype.f_is_lines = 64;
    rglwidgetClass.prototype.f_sprites_3d = 128;
    rglwidgetClass.prototype.f_is_subscene = 256;
    rglwidgetClass.prototype.f_is_clipplanes = 512;
    rglwidgetClass.prototype.f_fixed_size = 1024;
    rglwidgetClass.prototype.f_is_points = 2048;
    rglwidgetClass.prototype.f_is_twosided = 4096;
    rglwidgetClass.prototype.f_fat_lines = 8192;
    rglwidgetClass.prototype.f_is_brush = 16384;
    rglwidgetClass.prototype.f_has_fog = 32768;
    
    rglwidgetClass.prototype.fogNone = 0;
    rglwidgetClass.prototype.fogLinear = 1;
    rglwidgetClass.prototype.fogExp = 2;
    rglwidgetClass.prototype.fogExp2 = 3;

    /**
     * Start the writeWebGL scene. This is only used by writeWebGL; rglwidget has
       no debug element and does the drawing in rglwidget.js.
     */
    rglwidgetClass.prototype.start = function() {
      if (typeof this.prefix !== "undefined") {
        this.debugelement = document.getElementById(this.prefix + "debug");
        this.debug("");
      }
      this.drag = 0;
      this.drawScene();
    };
