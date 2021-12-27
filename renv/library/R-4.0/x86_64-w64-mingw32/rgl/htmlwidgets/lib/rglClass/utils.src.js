
    /**
     * Multiply matrix by vector
     * @returns {number[]}
     * @param M {number[][]} Left operand
     * @param v {number[]} Right operand
     */
    rglwidgetClass.prototype.multMV = function(M, v) {
        return [ M.m11 * v[0] + M.m12 * v[1] + M.m13 * v[2] + M.m14 * v[3],
                 M.m21 * v[0] + M.m22 * v[1] + M.m23 * v[2] + M.m24 * v[3],
                 M.m31 * v[0] + M.m32 * v[1] + M.m33 * v[2] + M.m34 * v[3],
                 M.m41 * v[0] + M.m42 * v[1] + M.m43 * v[2] + M.m44 * v[3]
               ];
    };
    
    /**
     * Multiply row vector by Matrix
     * @returns {number[]}
     * @param v {number[]} left operand
     * @param M {number[][]} right operand
     */
    rglwidgetClass.prototype.multVM = function(v, M) {
        return [ M.m11 * v[0] + M.m21 * v[1] + M.m31 * v[2] + M.m41 * v[3],
                 M.m12 * v[0] + M.m22 * v[1] + M.m32 * v[2] + M.m42 * v[3],
                 M.m13 * v[0] + M.m23 * v[1] + M.m33 * v[2] + M.m43 * v[3],
                 M.m14 * v[0] + M.m24 * v[1] + M.m34 * v[2] + M.m44 * v[3]
               ];
    };
    
    /**
     * Euclidean length of a vector
     * @returns {number}
     * @param v {number[]}
     */
    rglwidgetClass.prototype.vlen = function(v) {
      return Math.sqrt(this.dotprod(v, v));
    };

    /**
     * Dot product of two vectors
     * @instance rglwidgetClass
     * @returns {number}
     * @param a {number[]}
     * @param b {number[]}
     */
    rglwidgetClass.prototype.dotprod = function(a, b) {
      return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
    };

    /**
     * Cross product of two vectors
     * @returns {number[]}
     * @param a {number[]}
     * @param b {number[]}
     */
    rglwidgetClass.prototype.xprod = function(a, b) {
      return [a[1]*b[2] - a[2]*b[1],
          a[2]*b[0] - a[0]*b[2],
          a[0]*b[1] - a[1]*b[0]];
    };

    /**
     * Bind vectors or matrices by columns
     * @returns {number[][]}
     * @param a {number[][]}
     * @param b {number[]|number[][]}
     */
    rglwidgetClass.prototype.cbind = function(a, b) {
      if (b.length < a.length)
        b = this.repeatToLen(b, a.length);
      else if (a.length < b.length)
        a = this.repeatToLen(a, b.length);
      return a.map(function(currentValue, index) {
            return currentValue.concat(b[index]);
      });
    };

    /**
     * Swap elements
     * @returns {any[]}
     * @param a {any[]}
     * @param i {number} Element to swap
     * @param j {number} Other element to swap
     */
    rglwidgetClass.prototype.swap = function(a, i, j) {
      var temp = a[i];
      a[i] = a[j];
      a[j] = temp;
    };

    /**
     * Flatten a matrix into a vector
     * @returns {any[]}
     * @param a {any[][]}
     */
    rglwidgetClass.prototype.flatten = function(arr, result) {
      var value;
      if (typeof result === "undefined") result = [];
      for (var i = 0, length = arr.length; i < length; i++) {
        value = arr[i];
        if (Array.isArray(value)) {
          this.flatten(value, result);
        } else {
          result.push(value);
        }
      }
      return result;
    };

    /**
     * set element of 1d or 2d array as if it was flattened.
     * Column major, zero based!
     * @returns {any[]|any[][]}
     * @param {any[]|any[][]} a - array
     * @param {number} i - element
     * @param {any} value
     */
    rglwidgetClass.prototype.setElement = function(a, i, value) {
      if (Array.isArray(a[0])) {
        var dim = a.length,
            col = Math.floor(i/dim),
            row = i % dim;
        a[row][col] = value;
      } else {
        a[i] = value;
      }
    };

    /**
     * Transpose an array
     * @returns {any[][]}
     * @param {any[][]} a
     */
    rglwidgetClass.prototype.transpose = function(a) {
      var newArray = [],
          n = a.length,
          m = a[0].length,
          i;
      for(i = 0; i < m; i++){
        newArray.push([]);
      }

      for(i = 0; i < n; i++){
        for(var j = 0; j < m; j++){
          newArray[j].push(a[i][j]);
        }
      }
      return newArray;
    };

    /**
     * Calculate sum of squares of a numeric vector
     * @returns {number}
     * @param {number[]} x
     */
    rglwidgetClass.prototype.sumsq = function(x) {
      var result = 0, i;
      for (i=0; i < x.length; i++)
        result += x[i]*x[i];
      return result;
    };

    /**
     * Convert a matrix to a CanvasMatrix4
     * @returns {CanvasMatrix4}
     * @param {number[][]|number[]} mat
     */
    rglwidgetClass.prototype.toCanvasMatrix4 = function(mat) {
      if (mat instanceof CanvasMatrix4)
        return mat;
      var result = new CanvasMatrix4();
      mat = this.flatten(this.transpose(mat));
      result.load(mat);
      return result;
    };

    /**
     * Convert an R-style numeric colour string to an rgb vector
     * @returns {number[]}
     * @param {string} s
     */
    /* jshint bitwise:false */ 
    rglwidgetClass.prototype.stringToRgb = function(s) {
      s = s.replace("#", "");
      var bigint = parseInt(s, 16);
      return [((bigint >> 16) & 255)/255,
              ((bigint >> 8) & 255)/255,
               (bigint & 255)/255];
    };
    /* jshint bitwise:true */
    /**
     * Which list does a particular id come from?
     * @returns { string }
     * @param {number} id The id to look up.
     */
    rglwidgetClass.prototype.whichList = function(id) {
      var obj = this.getObj(id),
          flags = obj.flags;
        if (obj.type === "light")
          return "lights";
        if (this.isSet(flags, this.f_is_subscene))
            return "subscenes";
        if (this.isSet(flags, this.f_is_clipplanes))
            return "clipplanes";
        if (this.isSet(flags, this.f_is_transparent))
            return "transparent";
        return "opaque";
    };
    
    /**
     * Take a component-by-component product of two 3 vectors
     * @returns {number[]}
     * @param {number[]} x
     * @param {number[]} y
     */
    rglwidgetClass.prototype.componentProduct = function(x, y) {
      if (typeof y === "undefined") {
        this.alertOnce("Bad arg to componentProduct");
      }
      var result = new Float32Array(3), i;
      for (i = 0; i<3; i++)
        result[i] = x[i]*y[i];
      return result;
    };

    /**
     * Get next higher power of two
     * @returns { number }
     * @param { number } value - input value
     */
    rglwidgetClass.prototype.getPowerOfTwo = function(value) {
      var pow = 1;
      while(pow<value) {
        pow *= 2;
      }
      return pow;
    };

    /**
     * Unique entries
     * @returns { any[] }
     * @param { any[] } arr - An array
     */
    rglwidgetClass.prototype.unique = function(arr) {
      arr = [].concat(arr);
      return arr.filter(function(value, index, self) {
        return self.indexOf(value) === index;
      });
    };

    /**
     * Shallow compare of arrays
     * @returns { boolean }
     * @param { any[] } a - An array
     * @param { any[] } b - Another array
     */
    rglwidgetClass.prototype.equalArrays = function(a, b) {
      return a === b || (a && b &&
                      a.length === b.length &&
                      a.every(function(v, i) {return v === b[i];}));
    };
    
    /**
     * Repeat an array to a desired length
     * @returns {any[]}
     * @param {any | any[]} arr The input array
     * @param {number} len The desired output length
     */
    rglwidgetClass.prototype.repeatToLen = function(arr, len) {
      arr = [].concat(arr);
      while (arr.length < len/2)
        arr = arr.concat(arr);
      return arr.concat(arr.slice(0, len - arr.length));
    };

    /**
     * Give a single alert message, not to be repeated.
     * @param {string} msg  The message to give.
     */
    rglwidgetClass.prototype.alertOnce = function(msg) {
      if (typeof this.alerted !== "undefined")
        return;
      this.alerted = true;
      alert(msg);
    };

    /**
     * Get an object by id number.
     * @returns { Object }
     * @param {number} id
     */
    rglwidgetClass.prototype.getObj = function(id) {
      if (typeof id !== "number") {
        this.alertOnce("getObj id is "+typeof id);
      }
      return this.scene.objects[id];
    };

    /**
     * Get ids of a particular type from a subscene or the whole scene
     * @returns { number[] }
     * @param {string} type What type of object?
     * @param {number} subscene  Which subscene?  If not given, find in the whole scene
     */
    rglwidgetClass.prototype.getIdsByType = function(type, subscene) {
      var
        result = [], i, self = this, ids;
      if (typeof subscene === "undefined") {
        Object.keys(this.scene.objects).forEach(
          function(key) {
            key = parseInt(key, 10);
            if (self.getObj(key).type === type)
              result.push(key);
          });
      } else {
        ids = this.getObj(subscene).objects;
        for (i=0; i < ids.length; i++) {
          if (this.getObj(ids[i]).type === type) {
            result.push(ids[i]);
          }
        }
      }
      return result;
    };

    /**
     * Get a particular material property for an obj
     * @returns { any }
     * @param {object} obj  Which object?
     * @param {string} property Which material property?
     */
    rglwidgetClass.prototype.getObjMaterial = function(obj, property) {
      var mat;
      if (typeof obj.material === "undefined")
        console.error("material undefined");
      mat = obj.material[property];
      if (typeof mat === "undefined")
          mat = this.scene.material[property];
      return mat;
    };
    
    /**
     * Get a particular material property for an id
     * @returns { any }
     * @param {number} id  Which object?
     * @param {string} property Which material property?
     */
    rglwidgetClass.prototype.getMaterial = function(id, property) {
      var obj = this.getObj(id);
      return this.getObjMaterial(obj, property);
    };

    rglwidgetClass.prototype.getAdj = function (pos, offset, text) {
      switch(pos) {
        case 1: return [0.5, 1 + offset];
        case 2: return [1 + offset/text.length, 0.5];
        case 3: return [0.5, -offset];
        case 4: return [-offset/text.length, 0.5];
      }
    };

    /**
     * Count clipping planes in a scene
     * @returns {number}
     */
    rglwidgetClass.prototype.countClipplanes = function() {
      return this.countObjs("clipplanes");
    };

    /**
     * Count lights in a scene
     * @returns { number }
     */
    rglwidgetClass.prototype.countLights = function() {
      return this.countObjs("light");
    };

    /**
     * Count objects of specific type in a scene
     * @returns { number }
     * @param { string } type - Type of object to count
     */
    rglwidgetClass.prototype.countObjs = function(type) {
      var self = this,
          bound = 0;

      Object.keys(this.scene.objects).forEach(
        function(key) {
          if (self.getObj(parseInt(key, 10)).type === type)
            bound = bound + 1;
        });
      return bound;
    };

    /**
     * Display a debug message
     * @param { string } msg - The message to display
     * @param { Object } [img] - Image to insert before message
     */
    rglwidgetClass.prototype.debug = function(msg, img) {
      if (typeof this.debugelement !== "undefined" && this.debugelement !== null) {
        this.debugelement.innerHTML = msg;
        if (typeof img !== "undefined") {
          this.debugelement.insertBefore(img, this.debugelement.firstChild);
        }
      } else if (msg !== "")
        alert(msg);
    };

    /**
     * If we are in an ioslides or slidy presentation, get the
     * DOM element of the current slide
     * @returns { Object }
     */
    rglwidgetClass.prototype.getSlide = function() {
      var result = this.el, done = false;
      while (result && !done && this.scene.context.rmarkdown) {
      	switch(this.scene.context.rmarkdown) {
          case "ioslides_presentation":
            if (result.tagName === "SLIDE") return result;
            break;
          case "slidy_presentation":
            if (result.tagName === "DIV" && result.classList.contains("slide"))
              return result;
            break;
          default: return null;
      	}
      	result = result.parentElement;
      }
      return null;
    };

    /**
     * Is this scene visible in the browser?
     * @returns { boolean }
     */
    rglwidgetClass.prototype.isInBrowserViewport = function() {
      var rect = this.canvas.getBoundingClientRect(),
          windHeight = (window.innerHeight || document.documentElement.clientHeight),
          windWidth = (window.innerWidth || document.documentElement.clientWidth);
      if (this.scene.context && this.scene.context.rmarkdown !== null) {
      	if (this.slide)
      	  return (this.scene.context.rmarkdown === "ioslides_presentation" &&
      	          this.slide.classList.contains("current")) ||
      	         (this.scene.context.rmarkdown === "slidy_presentation" &&
      	          !this.slide.classList.contains("hidden"));
      }
      return (
      	rect.top >= -windHeight &&
      	rect.left >= -windWidth &&
      	rect.bottom <= 2*windHeight &&
      	rect.right <= 2*windWidth);
    };
    
    rglwidgetClass.prototype.keydiff = function(obj1, obj2) {
      var keys = Object.keys(obj1), i, result = [];
      for (i=0;i<keys.length;i++) {
        if (typeof obj1[keys[i]] !== "undefined" &&
            typeof obj2[keys[i]] === "undefined")
          result.push(keys[i]);
      }
      return result;
    };

    rglwidgetClass.prototype.isSet = function(flags, flag) {
      /* jshint bitwise: false */
      return (flags & flag) !== 0;
      /* jshint bitwise: true */
    };
