
    /**
     * Initial test for WebGL
     */
    rglwidgetClass.prototype.initGL0 = function() {
      if (!window.WebGLRenderingContext){
        this.alertOnce("Your browser does not support WebGL. See http://get.webgl.org");
        return;
      }
    };

    /**
     * Initialize WebGL
     * @returns { Object } the WebGL context
     */
    rglwidgetClass.prototype.initGL = function() {
      var self = this, success = false;
      if (this.gl) {
      	if (!this.drawing && this.gl.isContextLost())
          this.restartCanvas();
        else
          return this.gl;
      }
      // if (!this.isInBrowserViewport()) return; Return what??? At this point we know this.gl is null.
      this.canvas.addEventListener("webglcontextrestored",
        this.onContextRestored, false);
      this.canvas.addEventListener("webglcontextlost",
        this.onContextLost, false);
      this.gl = this.canvas.getContext("webgl", this.webGLoptions) ||
               this.canvas.getContext("experimental-webgl", this.webGLoptions);
      success = !!(this.gl && this.gl instanceof WebGLRenderingContext);
      if (!success)
        this.alertOnce("Your browser does not support WebGL. See http://get.webgl.org"); 
      this.index_uint = this.gl.getExtension("OES_element_index_uint");
      var save = this.startDrawing();
      Object.keys(this.scene.objects).forEach(function(key){
        self.initObj(parseInt(key, 10));
        });
      this.stopDrawing(save);
      return this.gl;
    };

    /**
     * Resize the display to match element
     * @param { Object } el - DOM element to match
     */
    rglwidgetClass.prototype.resize = function(el) {
      this.canvas.width = el.width;
      this.canvas.height = el.height;
    };

    /**
     * Initialize the sphere object
     */
    rglwidgetClass.prototype.initSphere = function(sections, segments) {
      var v = [], phi = [], theta = [], it = [], centers = [],
           i, j, k, ind, mod1, pole, result = {};
       
      for (i = 0; i < sections - 1; i++) {
        phi.push((i + 1)/sections - 0.5);
      }

      for (j = 0; j < segments; j++) {
        theta.push(2*j/segments);
        for (i = 0; i < sections - 1; i++) {
          /* These are [x,y,z,s,t]: */
          v.push([Math.sin(Math.PI*theta[j]) * Math.cos(Math.PI*phi[i]),
                  Math.sin(Math.PI*phi[i]),
                  Math.cos(Math.PI*theta[j]) * Math.cos(Math.PI*phi[i]),                               
                  theta[j]/2,
                  phi[i] + 0.5]);
        }
      }
      pole = v.length;
      v.push([0, -1, 0, 0, 0]); 
      v.push([0,  1, 0, 0, 1]);
      result.values = new Float32Array(this.flatten(v));
      result.vertexCount = v.length;
      
      mod1 = segments*(sections - 1);
      for (j = 0; j < segments; j++) {
        for (i = 0; i < sections - 2; i++) {
          ind = i + (sections - 1)*j;
          it.push([ind % mod1, 
                   (ind + sections - 1) % mod1,
                   (ind + sections) % mod1]);
          it.push([ind % mod1, 
                   (ind + sections) % mod1,
                   (ind + 1) % mod1]);
        }
        it.push([pole, 
                 ((j + 1)*(sections - 1)) % mod1,
                 ((j + 1)*(sections - 1) - sections + 1) % mod1]);
        it.push([pole + 1, 
                 ((j + 1)*(sections - 1) - 1) % mod1,
                 ((j + 1)*(sections - 1) + sections - 2) % mod1]);
      }
      result.it = new Uint16Array(this.flatten(it));
      
      for (i = 0; i < it.length; i++) {
        centers.push([0,0,0]);
        for (j = 0; j < 3; j++) { // x,y,z
          for (k = 0; k < 3; k++) {// vertices
            centers[i][j] += v[it[i][k]][j]/3;
          }
        }
      }
      result.centers = centers;
      
      result.vOffsets = {vofs:0, cofs:-1, nofs:0, radofs:-1, oofs:-1,
                         tofs:3, nextofs:-1, pointofs:-1, stride:5};

      result.f = [];
      result.indices = {};

      result.colorCount = 1;
      result.type = "sphere";
      this.sphere = result;
      this.initSphereGL();
    };

    /**
     * Do the gl part of initializing the sphere
     */
    rglwidgetClass.prototype.initSphereGL = function() {
      var gl = this.gl || this.initGL(), sphere = this.sphere;
      if (gl.isContextLost()) return;
      sphere.buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, sphere.buf);
      gl.bufferData(gl.ARRAY_BUFFER, sphere.values, gl.STATIC_DRAW);
      sphere.ibuf = [gl.createBuffer(), gl.createBuffer()];
      return;
    };

    /* Initialize common sphere object from spheres object
    */
    rglwidgetClass.prototype.initSphereFromObj = function(obj) {
      var i, pass, f, mode, sphere = this.sphere;
      sphere.ofsLoc = obj.ofsLoc;
      sphere.texLoc = obj.texLoc;
      sphere.sampler = obj.sampler;
      sphere.uFogMode = obj.uFogMode;
      sphere.uFogColor = obj.uFogColor;
      sphere.uFogParms = obj.uFogParms;
      sphere.userAttribLocations = obj.userAttribLocations;
      sphere.userUniformLocations = obj.userUniformLocations;
      sphere.normLoc = obj.normLoc;
      sphere.clipLoc = obj.clipLoc;
      sphere.nextLoc = obj.nextLoc;
      sphere.pointLoc = obj.pointLoc;
      sphere.aspectLoc = obj.aspectLoc;
      sphere.lwdLoc = obj.lwdLoc;
      sphere.prog = obj.prog;
      sphere.material = obj.material;
      sphere.flags = obj.flags;
      sphere.someHidden = obj.someHidden;
      sphere.fastTransparency = obj.fastTransparency;
      sphere.nlights = obj.nlights;
      sphere.emission = obj.emission;
      sphere.emissionLoc = obj.emissionLoc;
      sphere.shininess = obj.shininess;
      sphere.shininessLoc = obj.shininessLoc;
      sphere.ambient = obj.ambient;
      sphere.ambientLoc = obj.ambientLoc;
      sphere.specular = obj.specular;
      sphere.specularLoc = obj.specularLoc;
      sphere.diffuse = obj.diffuse;
      sphere.diffuseLoc = obj.diffuseLoc;
      sphere.lightDir = obj.lightDir;
      sphere.lightDirLoc = obj.lightDirLoc;
      sphere.viewpoint = obj.viewpoint;
      sphere.viewpointLoc = obj.viewpointLoc;
      sphere.finite = obj.finite;
      sphere.finiteLoc = obj.finiteLoc;
      sphere.prMatLoc = obj.prMatLoc;
      sphere.mvMatLoc = obj.mvMatLoc;
      sphere.normMatLoc = obj.normMatLoc;
      sphere.frontLoc = obj.frontLoc;
      sphere.index_uint = false;
      sphere.is_transparent = obj.is_transparent;
      sphere.ignoreExtent = obj.ignoreExtent;
      if (sphere.passes !== obj.passes ||
          JSON.stringify(sphere.pmode) !== JSON.stringify(obj.pmode)) {
        sphere.passes = obj.passes;
        sphere.pmode = obj.pmode;
        for (pass = 0; pass < obj.passes; pass++) {
          mode = sphere.pmode[pass];
          if (typeof sphere.indices[mode] === "undefined") {
            f = [];
            switch (mode) {
            case "culled": break;
            case "points":
              f.length = sphere.vertexCount;
              for (i=0; i < f.length; i++)
                f[i] = i;
              break;
            case "lines":
              f.length = 2*sphere.it.length;
      	      for (i=0; i < sphere.it.length/3; i++) {
      	        f[6*i] = sphere.it[3*i];
      	        f[6*i + 1] = sphere.it[3*i + 1];
      	        f[6*i + 2] = sphere.it[3*i + 1];
      	        f[6*i + 3] = sphere.it[3*i + 2];
      	        f[6*i + 4] = sphere.it[3*i + 2];
      	        f[6*i + 5] = sphere.it[3*i];
      	      }
      	      break;
      	    case "filled":
      	      f = sphere.it;
      	    }              
            sphere.indices[mode] = new Uint16Array(f);
          }
          sphere.f[pass] = sphere.indices[mode];
        }
      }
      // console.log("Names in spheres not in sphere:"+JSON.stringify(this.keydiff(obj, sphere)));
      sphere.initialized = true;
    };

    /**
     * Initialize a subscene
     * @param { number } id - id of subscene.
     */
    rglwidgetClass.prototype.initSubscene = function(id) {
      var sub = this.getObj(id),
          i, obj;

      if (sub.type !== "subscene")
        return;

      sub.par3d.userMatrix = this.toCanvasMatrix4(sub.par3d.userMatrix);
      sub.par3d.userProjection = this.toCanvasMatrix4(sub.par3d.userProjection);
      sub.par3d.userProjection.transpose();
      sub.par3d.listeners = [].concat(sub.par3d.listeners);
      sub.backgroundId = undefined;
      sub.subscenes = [];
      sub.clipplanes = [];
      sub.transparent = [];
      sub.opaque = [];
      sub.lights = [];
      for (i=0; i < sub.objects.length; i++) {
        obj = this.getObj(sub.objects[i]);
        if (typeof obj === "undefined") {
          sub.objects.splice(i, 1);
          i--;
        } else if (obj.type === "background")
          sub.backgroundId = obj.id;
        else
          sub[this.whichList(obj.id)].push(obj.id);
      }
    };

    /**
     * Initialize object for display
     * @param { number } id - id of object to initialize
     */
    rglwidgetClass.prototype.initObj = function(id) {
      var obj = this.getObj(id),
          flags = obj.flags,
          type = obj.type,
          is_lit = this.isSet(flags, this.f_is_lit),
          fat_lines = this.isSet(flags, this.f_fat_lines),
          has_texture = this.isSet(flags, this.f_has_texture),
          fixed_quads = this.isSet(flags, this.f_fixed_quads),
          is_transparent = this.isSet(flags, this.f_is_transparent),
          depth_sort = this.isSet(flags, this.f_depth_sort),
          sprites_3d = this.isSet(flags, this.f_sprites_3d),
          fixed_size = this.isSet(flags, this.f_fixed_size),
          is_twosided = this.isSet(flags, this.f_is_twosided),
          is_brush = this.isSet(flags, this.f_is_brush),
          has_fog = this.isSet(flags, this.f_has_fog),
          has_normals = typeof obj.normals !== "undefined", 
          gl = this.gl || this.initGL(),
          polygon_offset,
          texinfo, drawtype, nclipplanes, f, nrows, oldrows,
          i,j,v,v1,v2, mat, uri, matobj, pass, pmode,
          dim, nx, nz, nrow;

    if (typeof id !== "number") {
      this.alertOnce("initObj id is "+typeof id);
    }

    obj.initialized = true;

    obj.someHidden = false; // used in selection
    obj.is_transparent = is_transparent;
    
    if (type === "bboxdeco" || type === "subscene")
      return;
      
    if (type === "spheres" && typeof this.sphere === "undefined")
      this.initSphere(16, 16);

    if (type === "light") {
      obj.ambient = new Float32Array(obj.colors[0].slice(0,3));
      obj.diffuse = new Float32Array(obj.colors[1].slice(0,3));
      obj.specular = new Float32Array(obj.colors[2].slice(0,3));
      obj.lightDir = new Float32Array(obj.vertices[0]);
      return;
    }

    if (type === "clipplanes") {
      obj.vClipplane = this.flatten(this.cbind(obj.normals, obj.offsets));
      return;
    }

    if (type === "background" && typeof obj.ids !== "undefined") {
      obj.quad = this.flatten([].concat(obj.ids));
      return;
    }

    polygon_offset = this.getMaterial(id, "polygon_offset");
    if (polygon_offset[0] !== 0 || polygon_offset[1] !== 0)
      obj.polygon_offset = polygon_offset;

    if (is_transparent) {
      depth_sort = ["triangles", "quads", "surface",
                    "spheres", "sprites", "text"].indexOf(type) >= 0;
    }
    
    if (is_brush)
      this.initSelection(id);

    if (typeof obj.vertices === "undefined")
      obj.vertices = [];

    v = obj.vertices;
    obj.vertexCount = v.length;
    if (!obj.vertexCount) return;

    if (is_twosided && !has_normals) {
      if (typeof obj.userAttributes === "undefined")
        obj.userAttributes = {};
      v1 = Array(v.length);
      v2 = Array(v.length);
      if (obj.type === "triangles" || obj.type === "quads") {
      	if (obj.type === "triangles")
      	  nrow = 3;
      	else
      	  nrow = 4;
        for (i=0; i<Math.floor(v.length/nrow); i++)
          for (j=0; j<nrow; j++) {
            v1[nrow*i + j] = v[nrow*i + ((j+1) % nrow)];
            v2[nrow*i + j] = v[nrow*i + ((j+2) % nrow)];
          }
      } else if (obj.type === "surface") {
        dim = obj.dim[0];
        nx = dim[0];
        nz = dim[1];
        for (j=0; j<nx; j++) {
          for (i=0; i<nz; i++) {
            if (i+1 < nz && j+1 < nx) {
              v2[j + nx*i] = v[j + nx*(i+1)];
              v1[j + nx*i] = v[j+1 + nx*(i+1)];
            } else if (i+1 < nz) {
              v2[j + nx*i] = v[j-1 + nx*i];
              v1[j + nx*i] = v[j + nx*(i+1)];
            } else {
              v2[j + nx*i] = v[j + nx*(i-1)];
              v1[j + nx*i] = v[j-1 + nx*(i-1)];
            }
          }
        }
      }
      obj.userAttributes.aPos1 = v1;
      obj.userAttributes.aPos2 = v2;
    }

    if (!sprites_3d) {
      if (gl.isContextLost()) return;
      obj.prog = gl.createProgram();
      gl.attachShader(obj.prog, this.getShader( gl.VERTEX_SHADER,
        this.getVertexShader(id) ));
      gl.attachShader(obj.prog, this.getShader( gl.FRAGMENT_SHADER,
                      this.getFragmentShader(id) ));
      //  Force aPos to location 0, aCol to location 1
      gl.bindAttribLocation(obj.prog, 0, "aPos");
      gl.bindAttribLocation(obj.prog, 1, "aCol");
      gl.linkProgram(obj.prog);
      var linked = gl.getProgramParameter(obj.prog, gl.LINK_STATUS);
      if (!linked) {

        // An error occurred while linking
        var lastError = gl.getProgramInfoLog(obj.prog);
        console.warn("Error in program linking:" + lastError);

        gl.deleteProgram(obj.prog);
        return;
      }
    }

    if (type === "text") {
      texinfo = this.drawTextToCanvas(obj.texts,
                                      this.flatten(obj.cex),
                                      this.flatten(obj.family),
                                      this.flatten(obj.family));
    }

    if (fixed_quads && !sprites_3d) {
      obj.ofsLoc = gl.getAttribLocation(obj.prog, "aOfs");
    }

    if (has_texture || type === "text") {
      if (!obj.texture)
        obj.texture = gl.createTexture();
      obj.texLoc = gl.getAttribLocation(obj.prog, "aTexcoord");
      obj.sampler = gl.getUniformLocation(obj.prog, "uSampler");
    }
    
    if (has_fog && !sprites_3d) {
      obj.uFogMode = gl.getUniformLocation(obj.prog, "uFogMode");
      obj.uFogColor = gl.getUniformLocation(obj.prog, "uFogColor");
      obj.uFogParms = gl.getUniformLocation(obj.prog, "uFogParms");
    }

    if (has_texture) {
      mat = obj.material;
      if (typeof mat.uri !== "undefined")
        uri = mat.uri;
      else if (typeof mat.uriElementId === "undefined") {
        matobj = this.getObj(mat.uriId);
        if (typeof matobj !== "undefined") {
          uri = matobj.material.uri;
        } else {
          uri = "";
        }
      } else
        uri = document.getElementById(mat.uriElementId).rglinstance.getObj(mat.uriId).material.uri;

      this.loadImageToTexture(uri, obj.texture);
    }

    if (type === "text") {
      this.handleLoadedTexture(obj.texture, this.textureCanvas);
    }

    var stride = 3, nc, cofs, nofs, radofs, oofs, tofs, vnew, fnew,
        nextofs = -1, pointofs = -1, alias, colors, key, selection,
        filter, adj, pos, offset, attr, last, options;

    obj.alias = undefined;
    
    colors = obj.colors;

    j = this.scene.crosstalk.id.indexOf(id);
    if (j >= 0) {
      key = this.scene.crosstalk.key[j];
      options = this.scene.crosstalk.options[j];
      colors = colors.slice(0); 
      for (i = 0; i < v.length; i++)
        colors[i] = obj.colors[i % obj.colors.length].slice(0);
      if ( (selection = this.scene.crosstalk.selection) &&
           (selection.length || !options.selectedIgnoreNone) )
        for (i = 0; i < v.length; i++) {
          if (!selection.includes(key[i])) {
            if (options.deselectedColor)
              colors[i] = options.deselectedColor.slice(0);
            colors[i][3] = colors[i][3]*options.deselectedFade;   /* default: mostly transparent if not selected */
          } else if (options.selectedColor)
            colors[i] = options.selectedColor.slice(0);
        }
      if ( (filter = this.scene.crosstalk.filter) )
        for (i = 0; i < v.length; i++) 
          if (!filter.includes(key[i])) {
            if (options.filteredColor)
              colors[i] = options.filteredColor.slice(0);
            colors[i][3] = colors[i][3]*options.filteredFade;   /* default: completely hidden if filtered */
          }
    }  
    if (obj.type === "spheres")
      obj.sphereColors = colors;
    
    nc = obj.colorCount = colors.length;
    if (nc > 1) {
      cofs = stride;
      stride = stride + 4;
      v = this.cbind(v, colors);
    } else {
      cofs = -1;
      obj.onecolor = this.flatten(colors);
    }

    if (has_normals) {
      nofs = stride;
      stride = stride + 3;
      v = this.cbind(v, typeof obj.pnormals !== "undefined" ? obj.pnormals : obj.normals);
    } else
      nofs = -1;

    if (typeof obj.radii !== "undefined") {
      radofs = stride;
      stride = stride + 1;
      // FIXME:  always concat the radii?
      if (obj.radii.length === v.length) {
        v = this.cbind(v, obj.radii);
      } else if (obj.radii.length === 1) {
        v = v.map(function(row) { return row.concat(obj.radii[0]);});
      }
    } else
      radofs = -1;
      
    // Add default indices
    f = Array(v.length);
    for (i = 0; i < v.length; i++)
      f[i] = i;
    obj.f = [f,f];

    if (type === "sprites" && !sprites_3d) {
      tofs = stride;
      stride += 2;
      oofs = stride;
      stride += 2;
      vnew = new Array(4*v.length);
      fnew = new Array(4*v.length);
      alias = new Array(v.length);
      var rescale = fixed_size ? 72 : 1,
          size = obj.radii, s = rescale*size[0]/2;
      last = v.length;
      f = obj.f[0];
      for (i=0; i < v.length; i++) {
        if (size.length > 1)
          s = rescale*size[i]/2;
        vnew[i]  = v[i].concat([0,0,-s,-s]);
        fnew[4*i] = f[i];
        vnew[last]= v[i].concat([1,0, s,-s]);
        fnew[4*i+1] = last++;
        vnew[last]= v[i].concat([1,1, s, s]);
        fnew[4*i+2] = last++;
        vnew[last]= v[i].concat([0,1,-s, s]);
        fnew[4*i+3] = last++;
        alias[i] = [last-3, last-2, last-1];
      }
      v = vnew;
      obj.vertexCount = v.length;
      obj.f = [fnew, fnew];
    } else if (type === "text") {
      tofs = stride;
      stride += 2;
      oofs = stride;
      stride += 2;
      vnew = new Array(4*v.length);
      f = obj.f[0];
      fnew = new Array(4*f.length);
      alias = new Array(v.length);
      last = v.length;
      adj = this.flatten(obj.adj);
      if (typeof obj.pos !== "undefined") {
        pos = this.flatten(obj.pos);
        offset = adj[0];
      }
      for (i=0; i < v.length; i++) {
        if (typeof pos !== "undefined")
          adj = this.getAdj(pos[i % pos.length], offset, obj.texts[i]);
        vnew[i]  = v[i].concat([0,-0.5]).concat(adj);
        fnew[4*i] = f[i];
        vnew[last] = v[i].concat([1,-0.5]).concat(adj);
        fnew[4*i+1] = last++;
        vnew[last] = v[i].concat([1, 1.5]).concat(adj);
        fnew[4*i+2] = last++;
        vnew[last] = v[i].concat([0, 1.5]).concat(adj);
        fnew[4*i+3] = last++;
        alias[i] = [last-3, last-2, last-1];
        for (j=0; j < 4; j++) {
          v1 = vnew[fnew[4*i+j]];
          v1[tofs+2] = 2*(v1[tofs]-v1[tofs+2])*texinfo.widths[i];
          v1[tofs+3] = 2*(v1[tofs+1]-v1[tofs+3])*texinfo.textHeights[i];
          v1[tofs] = (texinfo.offsetsx[i] + v1[tofs]*texinfo.widths[i])/texinfo.canvasX;
          v1[tofs+1] = 1.0-(texinfo.offsetsy[i] -
              v1[tofs+1]*texinfo.textHeights[i])/texinfo.canvasY;
          vnew[fnew[4*i+j]] = v1;
        }
      }
      v = vnew;
      obj.vertexCount = v.length;
      obj.f = [fnew, fnew];
    } else if (typeof obj.texcoords !== "undefined") {
      tofs = stride;
      stride += 2;
      oofs = -1;
      v = this.cbind(v, obj.texcoords);
    } else {
      tofs = -1;
      oofs = -1;
    }
    
    obj.alias = alias;
                          
    if (typeof obj.userAttributes !== "undefined") {
      obj.userAttribOffsets = {};
      obj.userAttribLocations = {};
      obj.userAttribSizes = {};
      for (attr in obj.userAttributes) {
      	obj.userAttribLocations[attr] = gl.getAttribLocation(obj.prog, attr);
      	if (obj.userAttribLocations[attr] >= 0) { // Attribute may not have been used
      	  obj.userAttribOffsets[attr] = stride;
      	  v = this.cbind(v, obj.userAttributes[attr]);
      	  stride = v[0].length;
      	  obj.userAttribSizes[attr] = stride - obj.userAttribOffsets[attr];
      	}
      }
    }

    if (typeof obj.userUniforms !== "undefined") {
      obj.userUniformLocations = {};
      for (attr in obj.userUniforms)
        obj.userUniformLocations[attr] = gl.getUniformLocation(obj.prog, attr);
    }

    if (sprites_3d) {
      obj.userMatrix = new CanvasMatrix4(obj.usermatrix);
      obj.objects = this.flatten([].concat(obj.ids));
      is_lit = false;
      for (i=0; i < obj.objects.length; i++)
        this.initObj(obj.objects[i]);
    }

    if (is_lit && !fixed_quads) {
       obj.normLoc = gl.getAttribLocation(obj.prog, "aNorm");
    }

    nclipplanes = this.countClipplanes();
    if (nclipplanes && !sprites_3d) {
      obj.clipLoc = [];
      for (i=0; i < nclipplanes; i++)
        obj.clipLoc[i] = gl.getUniformLocation(obj.prog,"vClipplane" + i);
    }

    if (is_lit) {
      obj.emissionLoc = gl.getUniformLocation(obj.prog, "emission");
      obj.emission = new Float32Array(this.stringToRgb(this.getMaterial(id, "emission")));
      obj.shininessLoc = gl.getUniformLocation(obj.prog, "shininess");
      obj.shininess = this.getMaterial(id, "shininess");
      obj.nlights = this.countLights();
      obj.ambientLoc = [];
      obj.ambient = new Float32Array(this.stringToRgb(this.getMaterial(id, "ambient")));
      obj.specularLoc = [];
      obj.specular = new Float32Array(this.stringToRgb(this.getMaterial(id, "specular")));
      obj.diffuseLoc = [];
      obj.lightDirLoc = [];
      obj.viewpointLoc = [];
      obj.finiteLoc = [];
      for (i=0; i < obj.nlights; i++) {
        obj.ambientLoc[i] = gl.getUniformLocation(obj.prog, "ambient" + i);
        obj.specularLoc[i] = gl.getUniformLocation(obj.prog, "specular" + i);
        obj.diffuseLoc[i] = gl.getUniformLocation(obj.prog, "diffuse" + i);
        obj.lightDirLoc[i] = gl.getUniformLocation(obj.prog, "lightDir" + i);
        obj.viewpointLoc[i] = gl.getUniformLocation(obj.prog, "viewpoint" + i);
        obj.finiteLoc[i] = gl.getUniformLocation(obj.prog, "finite" + i);
      }
    }
    
    obj.passes = is_twosided + 1;
    obj.pmode = new Array(obj.passes);
    for (pass = 0; pass < obj.passes; pass++) {
      if (type === "triangles" || type === "quads" || type === "surface" || type === "spheres")
      	pmode = this.getMaterial(id, (pass === 0) ? "front" : "back");
      else pmode = "filled";
      obj.pmode[pass] = pmode;
    }
    if (type !== "spheres") {
      obj.f.length = obj.passes;
      for (pass = 0; pass < obj.passes; pass++) {
      	f = fnew = obj.f[pass];
        pmode = obj.pmode[pass];
      	if (pmode === "culled")
      	  f = [];
        else if (pmode === "points") {
          // stay with default
        } else if ((type === "quads" || type === "text" ||
             type === "sprites") && !sprites_3d) {
          nrows = Math.floor(obj.vertexCount/4);
          if (pmode === "filled") {
            fnew = Array(6*nrows);
            for (i=0; i < nrows; i++) {
              fnew[6*i] = f[4*i];
              fnew[6*i+1] = f[4*i + 1];
              fnew[6*i+2] = f[4*i + 2];
              fnew[6*i+3] = f[4*i];
              fnew[6*i+4] = f[4*i + 2];
              fnew[6*i+5] = f[4*i + 3];
            }
          } else {
            fnew = Array(8*nrows);
            for (i=0; i < nrows; i++) {
              fnew[8*i] = f[4*i];
              fnew[8*i+1] = f[4*i + 1];
              fnew[8*i+2] = f[4*i + 1];
              fnew[8*i+3] = f[4*i + 2];
              fnew[8*i+4] = f[4*i + 2];
              fnew[8*i+5] = f[4*i + 3];
              fnew[8*i+6] = f[4*i + 3];
              fnew[8*i+7] = f[4*i];
            }
          }
        } else if (type === "triangles") {
          nrows = Math.floor(obj.vertexCount/3);
          if (pmode === "filled") {
            fnew = Array(3*nrows);
            for (i=0; i < fnew.length; i++) {
              fnew[i] = f[i];
            }
          } else if (pmode === "lines") {
            fnew = Array(6*nrows);
      	    for (i=0; i < nrows; i++) {
      	      fnew[6*i] = f[3*i];
      	      fnew[6*i + 1] = f[3*i + 1];
      	      fnew[6*i + 2] = f[3*i + 1];
      	      fnew[6*i + 3] = f[3*i + 2];
      	      fnew[6*i + 4] = f[3*i + 2];
      	      fnew[6*i + 5] = f[3*i];
      	    }
          }
        } else if (type === "spheres") {
          // default
        } else if (type === "surface") {
          dim = obj.dim[0];
          nx = dim[0];
          nz = dim[1];
          if (pmode === "filled") {
            fnew = [];
            for (j=0; j<nx-1; j++) {
              for (i=0; i<nz-1; i++) {
                fnew.push(f[j + nx*i],
                       f[j + nx*(i+1)],
                       f[j + 1 + nx*(i+1)],
                       f[j + nx*i],
                       f[j + 1 + nx*(i+1)],
                       f[j + 1 + nx*i]);
              }
            }
          } else if (pmode === "lines") {
            fnew = [];
            for (j=0; j<nx; j++) {
              for (i=0; i<nz; i++) {
                if (i+1 < nz)
                  fnew.push(f[j + nx*i],
                         f[j + nx*(i+1)]);
                if (j+1 < nx)
                  fnew.push(f[j + nx*i],
                         f[j+1 + nx*i]);
              }
            }
          }
        }
        obj.f[pass] = fnew;
        if (depth_sort) {
          drawtype = "DYNAMIC_DRAW";
        } else {
          drawtype = "STATIC_DRAW";
        }
      }
    }
    
    if (fat_lines) {
      alias = undefined;
      obj.nextLoc = gl.getAttribLocation(obj.prog, "aNext");
      obj.pointLoc = gl.getAttribLocation(obj.prog, "aPoint");
      obj.aspectLoc = gl.getUniformLocation(obj.prog, "uAspect");
      obj.lwdLoc = gl.getUniformLocation(obj.prog, "uLwd");
      // Expand vertices to turn each segment into a pair of triangles
        
      	for (pass = 0; pass < obj.passes; pass++) {
      	  f = obj.f[pass];	
          oldrows = f.length;
      	  if (obj.pmode[pass] === "lines") 
      	    break;
      	}
      
      if (type === "linestrip") 
        nrows = 4*(oldrows - 1); 
      else
        nrows = 2*oldrows;
      vnew = new Array(nrows);
      fnew = new Array(1.5*nrows);
      
      // We're going to turn each pair of vertices into 4 new ones, with the "next" and "pt" attributes
      // added.
      // We do this by copying the originals in the first pass, adding the new attributes, then in a 
      // second pass add new vertices at the end.

      for (i = 0; i < v.length; i++) {
        vnew[i] = v[i].concat([0,0,0,0,0]); 
      }

      nextofs = stride;
      pointofs = stride + 3;
      stride = stride + 5;
            
      // Now add the extras
      var ind, k;
      last = v.length - 1;
      ind = 0;
      alias = new Array(f.length);
      for (i = 0; i < f.length; i++)
        alias[i] = [];
      for (i = 0; i < f.length - 1; i++) {
      	if (type !== "linestrip" && i % 2 === 1)
      	  continue;
      	k = ++last;
      	vnew[k] = vnew[f[i]].slice();
      	for (j=0; j<3; j++)
      	  vnew[k][nextofs + j] = vnew[f[i+1]][j];
      	vnew[k][pointofs] = -1;
      	vnew[k][pointofs+1] = -1;
      	fnew[ind] = k;
      	last++;
      	vnew[last] = vnew[k].slice();
      	vnew[last][pointofs] = 1;
      	fnew[ind+1] = last;
      	alias[f[i]].push(last-1, last);
      	last++;
      	k = last;
      	vnew[k] = vnew[f[i+1]].slice();
      	for (j=0; j<3; j++)
      	  vnew[k][nextofs + j] = vnew[f[i]][j];
      	vnew[k][pointofs] = -1;
      	vnew[k][pointofs+1] = 1;
      	fnew[ind+2] = k;
      	fnew[ind+3] = fnew[ind+1];
      	last++;
      	vnew[last] = vnew[k].slice();
      	vnew[last][pointofs] = 1;
      	fnew[ind+4] = last;
      	fnew[ind+5] = fnew[ind+2];
      	ind += 6;
      	alias[f[i+1]].push(last-1, last);
      }
      vnew.length = last+1;
      v = vnew;
      obj.vertexCount = v.length;
      if (typeof alias !== "undefined" && typeof obj.alias !== "undefined") {  // Already have aliases from previous section?
        var oldalias = obj.alias, newalias = Array(obj.alias.length);
        for (i = 0; i < newalias.length; i++) {
          newalias[i] = oldalias[i].slice();
          for (j = 0; j < oldalias[i].length; j++)
            Array.prototype.push.apply(newalias[i], alias[oldalias[j]]); // pushes each element 
        }
        obj.alias = newalias;
      } else
        obj.alias = alias;
      
      for (pass = 0; pass < obj.passes; pass++)
      	if (type === "lines" || type === "linestrip" || obj.pmode[pass] === "lines") {
          obj.f[pass] = fnew;
        }
      
      if (depth_sort) 
        drawtype = "DYNAMIC_DRAW";
      else
        drawtype = "STATIC_DRAW";
    }
    
      for (pass = 0; pass < obj.passes; pass++) {
        if (obj.vertexCount > 65535) {
          if (this.index_uint) {
            obj.f[pass] = new Uint32Array(obj.f[pass]);
            obj.index_uint = true;
          } else
            this.alertOnce("Object has "+obj.vertexCount+" vertices, not supported in this browser.");
        } else {
          obj.f[pass] = new Uint16Array(obj.f[pass]);
          obj.index_uint = false;
        }
      }
    
    if (stride !== v[0].length) {
      this.alertOnce("problem in stride calculation");
    }

    obj.vOffsets = {vofs:0, cofs:cofs, nofs:nofs, radofs:radofs, oofs:oofs, tofs:tofs,
                    nextofs:nextofs, pointofs:pointofs, stride:stride};

    obj.values = new Float32Array(this.flatten(v));

    if (type !== "spheres" && !sprites_3d) {
      obj.buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
      gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW); //
      obj.ibuf = Array(obj.passes);
      obj.ibuf[0] = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[0]);
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[0], gl[drawtype]);
      if (is_twosided) {
      	obj.ibuf[1] = gl.createBuffer();
      	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[1]);
      	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[1], gl[drawtype]);
      }
    }

    if (!sprites_3d) {
      obj.mvMatLoc = gl.getUniformLocation(obj.prog, "mvMatrix");
      obj.prMatLoc = gl.getUniformLocation(obj.prog, "prMatrix");
    }

    if (fixed_size) {
      obj.textScaleLoc = gl.getUniformLocation(obj.prog, "textScale");
    }

    if (is_lit && !sprites_3d) {
      obj.normMatLoc = gl.getUniformLocation(obj.prog, "normMatrix");
    }

    if (is_twosided) {
      obj.frontLoc = gl.getUniformLocation(obj.prog, "front");
    }
  };
    
    /**
     * Initialize the DOM object
     * @param { Object } el - the DOM object
     * @param { Object } x - the scene data sent by JSON from R
     */
    rglwidgetClass.prototype.initialize = function(el, x) {
      this.textureCanvas = document.createElement("canvas");
      this.textureCanvas.style.display = "block";
      this.scene = x;
      this.normMatrix = new CanvasMatrix4();
      this.saveMat = {};
      this.distance = null;
      this.posLoc = 0;
      this.colLoc = 1;
      if (el) {
        el.rglinstance = this;
        this.el = el;
        this.webGLoptions = el.rglinstance.scene.webGLoptions;
        this.initCanvas();
      }
      if (typeof Shiny !== "undefined") {
        var self = this;
        Shiny.addCustomMessageHandler("shinyGetPar3d",
          function(message) {
            var i, param, 
                subscene = self.getObj(message.subscene),
                parameters = [].concat(message.parameters),
                result = {tag: message.tag, subscene: message.subscene};
            if (typeof subscene !== "undefined") {
              for (i = 0; i < parameters.length; i++) {
                param = parameters[i];
                result[param] = subscene.par3d[param];
              }
            } else {
              console.log("subscene "+message.subscene+" undefined.");
            }
            Shiny.setInputValue("par3d:shinyPar3d", result, {priority: "event"});
          });
          
        Shiny.addCustomMessageHandler("shinySetPar3d",
          function(message) {
            var param = message.parameter, 
                subscene = self.getObj(message.subscene);
            if (typeof subscene !== "undefined") {
              subscene.par3d[param] = message.value;
              subscene.initialized = false;
              self.drawScene();
            } else {
              console.log("subscene "+message.subscene+" undefined.");
            }
          });
          
        Shiny.addCustomMessageHandler("resetBrush",
          function(message) {
            if (message === self.scene.selectionInput) {
              self.clearBrush(null);
              self.recordSelection(0);
            }
          });
      }
    };
    
    /**
     * Restart the WebGL canvas
     */
    rglwidgetClass.prototype.restartCanvas = function() {
      var newcanvas = document.createElement("canvas"),
          self = this;
      newcanvas.width = this.el.width;
      newcanvas.height = this.el.height;
      newcanvas.addEventListener("webglcontextrestored",
        this.onContextRestored, false);
      newcanvas.addEventListener("webglcontextlost",
        this.onContextLost, false);
      while (this.el.firstChild) {
        this.el.removeChild(this.el.firstChild);
      }
      this.el.appendChild(newcanvas);
      this.canvas = newcanvas;
      this.setMouseHandlers();
      if (this.gl) 
        Object.keys(this.scene.objects).forEach(function(key){
          self.getObj(parseInt(key, 10)).texture = undefined; 
          });
      this.gl = null;
    };

    /**
     * Initialize the WebGL canvas
     */
    rglwidgetClass.prototype.initCanvas = function() {
      this.restartCanvas();
      var objs = this.scene.objects,
          self = this;
      Object.keys(objs).forEach(function(key){
        self.initSubscene(parseInt(key, 10));
      });
      this.setMouseHandlers();

      this.onContextRestored = function() {
        self.initGL();
        self.drawScene();
      };

      this.onContextLost = function(event) {
        if (!self.drawing)
          this.gl = null;
        event.preventDefault();
      };

      this.initGL0();
      this.lazyLoadScene = function() {
      	if (typeof self.slide === "undefined")
      	  self.slide = self.getSlide();
      	if (self.isInBrowserViewport()) {
      	  if (!self.gl || self.gl.isContextLost())
      	    self.initGL();
      	  self.drawScene();
      	}
      };
      window.addEventListener("DOMContentLoaded", this.lazyLoadScene, false);
      window.addEventListener("load", this.lazyLoadScene, false);
      window.addEventListener("resize", this.lazyLoadScene, false);
      window.addEventListener("scroll", this.lazyLoadScene, false);
      this.slide = this.getSlide();
      if (this.slide) {
        if (typeof this.slide.rgl === "undefined")
          this.slide.rgl = [this];
        else
          this.slide.rgl.push(this);
        if (this.scene.context.rmarkdown) 
          if (this.scene.context.rmarkdown === "ioslides_presentation") {
            this.slide.setAttribute("slideenter", "this.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window);})");
          } else if (this.scene.context.rmarkdown === "slidy_presentation") {
            // This method would also work in ioslides, but it gets triggered
            // something like 5 times per slide for every slide change, so
            // you'd need a quicker function than lazyLoadScene.
            var MutationObserver = window.MutationObserver || window.WebKitMutationObserver || window.MozMutationObserver,
            observer = new MutationObserver(function(mutations) {
              mutations.forEach(function() {
                self.slide.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window); });});});
            observer.observe(this.slide, { attributes: true, attributeFilter:["class"] });
          }
      }
    };
