
    /**
     * Start drawing
     * @returns { boolean } Previous state
     */
    rglwidgetClass.prototype.startDrawing = function() {
    	var value = this.drawing;
    	this.drawing = true;
    	return value;
    };

    /**
     * Stop drawing and check for context loss
     * @param { boolean } saved - Previous state
     */
    rglwidgetClass.prototype.stopDrawing = function(saved) {
      this.drawing = saved;
      if (!saved && this.gl && this.gl.isContextLost())
        this.restartCanvas();
    };

    /**
     * Update the triangles used to display a plane
     * @param { number } id - id of the plane
     * @param { Object } bbox - bounding box in which to display the plane
     */
    rglwidgetClass.prototype.planeUpdateTriangles = function(obj, bbox) {
      var perms = [[0,0,1], [1,2,2], [2,1,0]],
          x, xrow, elem, A, d, nhits, i, j, k, u, v, w, intersect, which, v0, v2, vx, reverse,
          face1 = [], face2 = [], normals = [],
          nPlanes = obj.normals.length;
      obj.bbox = bbox;
      obj.vertices = [];
      obj.initialized = false;
      for (elem = 0; elem < nPlanes; elem++) {
//    Vertex Av = normal.getRecycled(elem);
        x = [];
        A = obj.normals[elem];
        d = obj.offsets[elem][0];
        nhits = 0;
        for (i=0; i<3; i++)
          for (j=0; j<2; j++)
            for (k=0; k<2; k++) {
              u = perms[0][i];
              v = perms[1][i];
              w = perms[2][i];
              if (A[w] !== 0.0) {
                intersect = -(d + A[u]*bbox[j+2*u] + A[v]*bbox[k+2*v])/A[w];
                if (bbox[2*w] < intersect && intersect < bbox[1+2*w]) {
                  xrow = [];
                  xrow[u] = bbox[j+2*u];
                  xrow[v] = bbox[k+2*v];
                  xrow[w] = intersect;
                  x.push(xrow);
                  face1[nhits] = j + 2*u;
                  face2[nhits] = k + 2*v;
                  nhits++;
                }
              }
            }

            if (nhits > 3) {
            /* Re-order the intersections so the triangles work */
              for (i=0; i<nhits-2; i++) {
                which = 0; /* initialize to suppress warning */
                for (j=i+1; j<nhits; j++) {
                  if (face1[i] === face1[j] || face1[i] === face2[j] ||
                      face2[i] === face1[j] || face2[i] === face2[j] ) {
                    which = j;
                    break;
                  }
                }
                if (which > i+1) {
                  this.swap(x, i+1, which);
                  this.swap(face1, i+1, which);
                  this.swap(face2, i+1, which);
                }
              }
            }
            if (nhits >= 3) {
      /* Put in order so that the normal points out the FRONT of the faces */
              v0 = [x[0][0] - x[1][0] , x[0][1] - x[1][1], x[0][2] - x[1][2]];
              v2 = [x[2][0] - x[1][0] , x[2][1] - x[1][1], x[2][2] - x[1][2]];
              /* cross-product */
              vx = this.xprod(v0, v2);
              reverse = this.dotprod(vx, A) > 0;

              for (i=0; i<nhits-2; i++) {
                obj.vertices.push(x[0]);
                normals.push(A);
                for (j=1; j<3; j++) {
                  obj.vertices.push(x[i + (reverse ? 3-j : j)]);
                  normals.push(A);
                }
              }
            }
      }
      obj.pnormals = normals;
    };
    
    rglwidgetClass.prototype.mode4type = {points : "POINTS",
                     linestrip : "LINE_STRIP",
                     abclines : "LINES",
                     lines : "LINES",
                     sprites : "TRIANGLES",
                     planes : "TRIANGLES",
                     text : "TRIANGLES",
                     quads : "TRIANGLES",
                     surface : "TRIANGLES",
                     triangles : "TRIANGLES",
                     sphere : "TRIANGLES"
    };

    rglwidgetClass.prototype.disableArrays = function(obj, enabled) {
      var gl = this.gl || this.initGL(),
          objLocs = ["normLoc", "texLoc", "ofsLoc", "pointLoc", "nextLoc"],
          thisLocs = ["posLoc", "colLoc"], i, attr;
      for (i = 0; i < objLocs.length; i++) 
        if (enabled[objLocs[i]]) gl.disableVertexAttribArray(obj[objLocs[i]]);
      for (i = 0; i < thisLocs.length; i++)
        if (enabled[thisLocs[i]]) gl.disableVertexAttribArray(this[objLocs[i]]);
      if (typeof obj.userAttributes !== "undefined") {
      	for (attr in obj.userAttribSizes) {  // Not all attributes may have been used
      	  gl.disableVertexAttribArray( obj.userAttribLocations[attr] );
      	}
      }
    };
    
    rglwidgetClass.prototype.doStartScene = function() {
      var gl = this.gl || this.initGL();
      gl.enable(gl.DEPTH_TEST);
      gl.depthFunc(gl.LEQUAL);
      gl.clearDepth(1.0);
      gl.clearColor(1,1,1,1);
      gl.depthMask(true); // Must be true before clearing depth buffer
      /* jshint bitwise: false */
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      /* jshint bitwise: true */
    };
    
    /**
     * Set gl depth test based on object's material
     * @param { object } obj - object to use
     */
    rglwidgetClass.prototype.doDepthTest = function(obj) {
      var gl = this.gl,
          tests = {never: gl.NEVER,
                   less:  gl.LESS,
                   equal: gl.EQUAL,
                   lequal:gl.LEQUAL,
                   greater: gl.GREATER,
                   notequal: gl.NOTEQUAL,
                   gequal: gl.GEQUAL,
                   always: gl.ALWAYS},
           test = tests[this.getObjMaterial(obj, "depth_test")];
      gl.depthFunc(test);
    };    
    
    /**
     * Set polygon offset for an obj
     * @param { object } obj - object to use
     */
    rglwidgetClass.prototype.doPolygonOffset = function(obj) { 
      var gl = this.gl;
      if (typeof obj.polygon_offset !== "undefined") {
        gl.polygonOffset(obj.polygon_offset[0],
                          obj.polygon_offset[1]);
        gl.enable(gl.POLYGON_OFFSET_FILL);
      } else
        gl.disable(gl.POLYGON_OFFSET_FILL);
    };
    
    rglwidgetClass.prototype.doClipping = function(obj, subscene) {
      var gl = this.gl,
          clipcheck = 0,
          clipplaneids = subscene.clipplanes,
          clip, i,j;
      for (i=0; i < clipplaneids.length; i++) {
        clip = this.getObj(clipplaneids[i]);
        for (j=0; j < clip.offsets.length; j++) {
          gl.uniform4fv(obj.clipLoc[clipcheck + j], clip.IMVClip[j]);
        }
        clipcheck += clip.offsets.length;
      }
      if (typeof obj.clipLoc !== "undefined")
        for (i=clipcheck; i < obj.clipLoc.length; i++)
          gl.uniform4f(obj.clipLoc[i], 0,0,0,0);
    };
    
    rglwidgetClass.prototype.doLighting = function(obj, subscene) {
      var gl = this.gl, i, light;
        gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(this.normMatrix.getAsArray()) );
        gl.uniform3fv( obj.emissionLoc, obj.emission);
        gl.uniform1f( obj.shininessLoc, obj.shininess);
        for (i=0; i < subscene.lights.length; i++) {
          light = this.getObj(subscene.lights[i]);
          if (!light.initialized) this.initObj(light.id);
          gl.uniform3fv( obj.ambientLoc[i], this.componentProduct(light.ambient, obj.ambient));
          gl.uniform3fv( obj.specularLoc[i], this.componentProduct(light.specular, obj.specular));
          gl.uniform3fv( obj.diffuseLoc[i], light.diffuse);
          gl.uniform3fv( obj.lightDirLoc[i], light.lightDir);
          gl.uniform1i( obj.viewpointLoc[i], light.viewpoint);
          gl.uniform1i( obj.finiteLoc[i], light.finite);
        }
        for (i=subscene.lights.length; i < obj.nlights; i++) {
          gl.uniform3f( obj.ambientLoc[i], 0,0,0);
          gl.uniform3f( obj.specularLoc[i], 0,0,0);
          gl.uniform3f( obj.diffuseLoc[i], 0,0,0);
        }
    };
    
    rglwidgetClass.prototype.doColors = function(obj) {
      var gl = this.gl;
      if (obj.colorCount === 1) {
        gl.disableVertexAttribArray( this.colLoc );
        gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
        return false;
      } else {
        gl.enableVertexAttribArray( this.colLoc );
        gl.vertexAttribPointer(this.colLoc, 4, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.cofs);
        return true;
      }
    };
    
    rglwidgetClass.prototype.doNormals = function(obj) {
      var gl = this.gl;
      if (obj.vOffsets.nofs >= 0) {
        gl.enableVertexAttribArray( obj.normLoc );
        gl.vertexAttribPointer(obj.normLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nofs);
        return true;
      } else
        return false;
    };
    
    rglwidgetClass.prototype.doTexture = function(obj) {
      var gl = this.gl, 
          is_spheres = obj.type === "spheres";
        gl.enableVertexAttribArray( obj.texLoc );
        if (is_spheres)
          gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*this.sphere.vOffsets.stride, 4*this.sphere.vOffsets.tofs);
        else
          gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.tofs);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, obj.texture);
        gl.uniform1i( obj.sampler, 0);
        return true;
    };
    
    rglwidgetClass.prototype.doUserAttributes = function(obj) {
      if (typeof obj.userAttributes !== "undefined") {
        var gl = this.gl;
      	for (var attr in obj.userAttribSizes) {  // Not all attributes may have been used
      	  gl.enableVertexAttribArray( obj.userAttribLocations[attr] );
      	  gl.vertexAttribPointer( obj.userAttribLocations[attr], obj.userAttribSizes[attr],
      	  			  gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.userAttribOffsets[attr]);
      	}
      }
    };
    
    rglwidgetClass.prototype.doUserUniforms = function(obj) {
      if (typeof obj.userUniforms !== "undefined") {
        var gl = this.gl;
      	for (var attr in obj.userUniformLocations) {
      	  var loc = obj.userUniformLocations[attr];
      	  if (loc !== null) {
      	    var uniform = obj.userUniforms[attr];
      	    if (typeof uniform.length === "undefined")
      	      gl.uniform1f(loc, uniform);
      	    else if (typeof uniform[0].length === "undefined") {
      	      uniform = new Float32Array(uniform);
      	      switch(uniform.length) {
      	      	case 2: gl.uniform2fv(loc, uniform); break;
      	      	case 3: gl.uniform3fv(loc, uniform); break;
      	      	case 4: gl.uniform4fv(loc, uniform); break;
      	      	default: console.warn("bad uniform length");
      	      }
      	    } else if (uniform.length === 4 && uniform[0].length === 4)
      	      gl.uniformMatrix4fv(loc, false, new Float32Array(uniform.getAsArray()));
      	    else
      	      console.warn("unsupported uniform matrix");
      	  }
      	}
      }
    };
    
    rglwidgetClass.prototype.doLoadIndices = function(obj, pass, indices) {
      var gl = this.gl,
          f = obj.f[pass],
          type = obj.type,
          fat_lines = this.isSet(obj.flags, this.f_fat_lines),
          fnew, step;
      switch(type){
        case "points":
          step = 1;
          break;
        case "abclines":
        case "lines":
          if (fat_lines)
            step = 6;
          else
            step = 2;
          break;
        case "linestrip":
          if (fat_lines)
            step = 6;
          else
            step = 1;
          break;
        case "sphere":
        case "planes":
        case "triangles":
          step = 3;
          break;
        case "text":
        case "sprites":
        case "quads":
        case "surface":
          step = 6;
          break;
        default:
          console.error("loadIndices for "+type);
          return 0;
      }
      if (obj.index_uint)
        fnew = new Uint32Array(step * indices.length);
      else
        fnew = new Uint16Array(step * indices.length);
      for (var i = 0; i < indices.length; i++) {
        for (var j = 0; j < step; j++) {
          fnew[step*i + j] = f[step*indices[i] + j];
        }
      }
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, fnew, gl.DYNAMIC_DRAW);
      return fnew.length;
    };

    rglwidgetClass.prototype.doMasking = function(mask) {
      var gl = this.gl;
      gl.depthMask(mask);
    };
    
    rglwidgetClass.prototype.doBlending = function(blend) {
      var gl = this.gl;
      if (blend) {
        gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA,
                           gl.ONE, gl.ONE);
        gl.enable(gl.BLEND);
      } else {
        gl.disable(gl.BLEND);
      }
    };
    
    /**
     * Set up for fog in the subscene
     * @param { number } id - id of background object
     */
    rglwidgetClass.prototype.doFog = function(obj, subscene) {
      var gl = this.gl, fogmode, color, 
          observer = subscene.par3d.observer[2],
          sintheta = Math.sin(subscene.par3d.FOV*Math.PI/180/2),
          parms = [this.frustum.near - 2*observer,
                   this.frustum.far - 2*observer,
                   this.fogScale,
                   (1-sintheta)/(1+sintheta)];
      if (typeof this.fogType === "undefined")
        this.fogType = "none";
      if (typeof this.fogScale === "undefined")
        parms[2] = 1;
      if (sintheta === 0)
        parms[3] = 1/3;
      switch(this.fogType){
        case "none": fogmode = 0; break;
        case "linear": 
          fogmode = 1; break;
        case "exp":  
          fogmode = 2; break;
        case "exp2": 
          fogmode = 3;
          break;
        default: console.error("Unknown fogtype "+this.fogType);
      }
      gl.uniform1i(obj.uFogMode, fogmode);
      color = this.fogColor;
      gl.uniform3f(obj.uFogColor, color[0], color[1], color[2]);
      gl.uniform4f(obj.uFogParms, parms[0], parms[1], parms[2], parms[3]);
    };

    /* The draw methods are called twice.  When 
       this.opaquePass is true, they should draw opaque parts
       of the scene, and return the list of transparent
       pieces.  Here context is the context array on input,
       modified when the matrices are changed.
       When this.opaquePass is false, the context argument
       contains a "piece", i.e. an ordered list of parts
       of the object to draw. */
       
    rglwidgetClass.prototype.drawSimple = function(obj, subscene, context) {
      var 
          flags = obj.flags,
          type = obj.type,
          is_lit = this.isSet(flags, this.f_is_lit),
          has_texture = this.isSet(flags, this.f_has_texture),
          is_transparent = this.isSet(flags, this.f_is_transparent),
          fixed_size = this.isSet(flags, this.f_fixed_size),
          fixed_quads = this.isSet(flags, this.f_fixed_quads),
          is_lines = this.isSet(flags, this.f_is_lines),
          fat_lines = this.isSet(flags, this.f_fat_lines),
          is_twosided = this.isSet(flags, this.f_is_twosided),
          has_fog = this.isSet(flags, this.f_has_fog),
          gl = this.gl || this.initGL(),
          count,
          pass, mode, pmode,
          enabled = {};

      if (!obj.initialized)
        this.initObj(obj.id);

      count = obj.vertexCount;
      if (!count)
        return [];
    
      is_transparent = is_transparent || obj.someHidden;
      
      if (is_transparent && this.opaquePass)
        return this.getPieces(context, obj.id, 0, obj);

      this.doDepthTest(obj);
      
      this.doMasking(this.getObjMaterial(obj, "depth_mask"));
            
      gl.useProgram(obj.prog);

      this.doPolygonOffset(obj);

      gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);

      gl.uniformMatrix4fv( obj.prMatLoc, false, new Float32Array(this.prMatrix.getAsArray()) );
      gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(this.mvMatrix.getAsArray()) );

      this.doClipping(obj, subscene);

      if (is_lit)
        this.doLighting(obj, subscene);

      if (has_fog)
        this.doFog(obj, subscene);

      this.doUserAttributes(obj);

      this.doUserUniforms(obj);
 
      gl.enableVertexAttribArray( this.posLoc );
      enabled.posLoc = true;
        
      if (has_texture || obj.type === "text")
        enabled.texLoc = this.doTexture(obj);

      enabled.colLoc = this.doColors(obj);
      if (is_lit)
        enabled.normLoc = this.doNormals(obj);

      if (fixed_size) {
        gl.uniform2f( obj.textScaleLoc, 0.75/this.vp.width, 0.75/this.vp.height);
      }
      
      if (fixed_quads) {
        gl.enableVertexAttribArray( obj.ofsLoc );
        enabled.ofsLoc = true;
        gl.vertexAttribPointer(obj.ofsLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.oofs);
      }

      for (pass = 0; pass < obj.passes; pass++) {
      	pmode = obj.pmode[pass];
        if (pmode === "culled")
          continue;

      	mode = fat_lines && (is_lines || pmode === "lines") ? "TRIANGLES" : this.mode4type[type];

      	if (is_twosided)
      	  gl.uniform1i(obj.frontLoc, pass !== 0);

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);
        if (!this.opaquePass) {
          if (type === "sphere" && obj.fastTransparency)
            count = this.doLoadIndices(obj, pass, this.sphere.fastpieces[0].indices);
          else
            count = this.doLoadIndices(obj, pass, context.indices);
        } else {
          count = obj.f[pass].length;
          gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[pass], gl.STATIC_DRAW);
        }
      	if (!is_lines && pmode === "lines" && !fat_lines) {
          mode = "LINES";
        } else if (pmode === "points") {
          mode = "POINTS";
        }
                          
        if ((is_lines || pmode === "lines") && fat_lines) {
          gl.enableVertexAttribArray(obj.pointLoc);
          enabled.pointLoc = true;
          gl.vertexAttribPointer(obj.pointLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.pointofs);
          gl.enableVertexAttribArray(obj.nextLoc );
          enabled.nextLoc = true;
          gl.vertexAttribPointer(obj.nextLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nextofs);
          gl.uniform1f(obj.aspectLoc, this.vp.width/this.vp.height);
          gl.uniform1f(obj.lwdLoc, this.getMaterial(obj.id, "lwd")/this.vp.height);
        }

        gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*obj.vOffsets.stride,  4*obj.vOffsets.vofs);

        gl.drawElements(gl[mode], count, obj.index_uint ? gl.UNSIGNED_INT : gl.UNSIGNED_SHORT, 0);
      }
      this.disableArrays(obj, enabled);
      return [];
    };
   
    rglwidgetClass.prototype.drawPlanes = function(obj, subscene, context) {
      if (obj.bbox !== subscene.par3d.bbox || !obj.initialized) {
          this.planeUpdateTriangles(obj, subscene.par3d.bbox);
      }
      return this.drawSimple(obj, subscene, context);
   };

    /**
     * Draw spheres in a subscene
     * @param { object } obj - object to draw
     * @param { object } subscene 
     * @param { object } context 
     */
     
    /**
     * Drawing spheres happens in six ways:
     * 1 opaquepass, not transparent:  transform and draw this.sphere count times
     * 2 opaquepass, transparent, not fast: transform & collect sphere pieces count times
     * 3 opaquepass, transparent, fast:  order the centres into separate pieces, order this.sphere once
     * 4 not opaquepass, not transparent:  do nothing
     * 5 not opaquepass, transparent, not fast:  transform for one sphere, draw one merged piece
     * 6 not opaquepass, transparent, fast:  transform for one sphere, draw this.sphere in fixed order.
     **/

    rglwidgetClass.prototype.drawSpheres = function(obj, subscene, context) {
      var flags = obj.flags,
          is_transparent = this.isSet(flags, this.f_is_transparent),
          sphereMV, baseofs, ofs, sscale, i,
          count, nc, scount, scale, indices, sphereNorm,
          enabled = {}, drawing,
          saveNorm = new CanvasMatrix4(this.normMatrix),
          saveMV = new CanvasMatrix4(this.mvMatrix),
          savePRMV = null,
          result = [], idx;

      if (!obj.initialized)
        this.initObj(obj.id);

      count = obj.vertexCount;
      if (!count)
        return result;
        
      is_transparent = is_transparent || obj.someHidden;

      if (!this.opaquePass && !is_transparent)
        return result;
        
      if (this.prmvMatrix !== null)
        savePRMV = new CanvasMatrix4(this.prmvMatrix);
      
      scale = subscene.par3d.scale;        
      sphereNorm = new CanvasMatrix4();
      sphereNorm.scale(scale[0], scale[1], scale[2]);
      sphereNorm.multRight(saveNorm);
      this.normMatrix = sphereNorm;

      if (this.opaquePass) {
        context = context.slice();
        context.push(obj.id);
      } 
      
      drawing = this.opaquePass !== is_transparent;
      if (drawing) {
        nc = obj.colorCount;
        if (nc === 1) {
          this.sphere.onecolor = obj.onecolor;
        }
      }
      
      this.initSphereFromObj(obj);

      if (!this.opaquePass && obj.fastTransparency && typeof this.sphere.fastpieces === "undefined") {
        this.sphere.fastpieces = this.getPieces(context.context, obj.id, 0, this.sphere);
        this.sphere.fastpieces = this.sortPieces(this.sphere.fastpieces);
        this.sphere.fastpieces = this.mergePieces(this.sphere.fastpieces);
      }

      if (this.opaquePass)
        scount = count;
      else {
        indices = context.indices;
        if (obj.fastTransparency)
          scount = indices.length;  /* Each item gives the center of a whole sphere */
        else
          scount = 1;               /* Each item is a fragment of the sphere, at location subid */
      }
      for (i = 0; i < scount; i++) {
        sphereMV = new CanvasMatrix4();
        if (this.opaquePass)
          idx = i;
        else if (obj.fastTransparency)
          idx = indices[i];
        else
          idx = context.subid;
        if (typeof idx === "undefined")
          console.error("idx is undefined");
        baseofs = idx*obj.vOffsets.stride;
        ofs = baseofs + obj.vOffsets.radofs;
        sscale = obj.values[ofs];

        sphereMV.scale(sscale/scale[0], sscale/scale[1], sscale/scale[2]);
        sphereMV.translate(obj.values[baseofs],
                             obj.values[baseofs+1],
                             obj.values[baseofs+2]);
        sphereMV.multRight(saveMV);
        this.mvMatrix = sphereMV;
        this.setprmvMatrix();
        if (drawing) {
          if (nc > 1) {
            this.sphere.onecolor = this.flatten(obj.sphereColors[idx % obj.sphereColors.length]);
          }
          this.drawSimple(this.sphere, subscene, context);
        } else 
          result = result.concat(this.getSpherePieces(context, i, obj));
      }
      if (drawing)
        this.disableArrays(obj, enabled);
      this.normMatrix = saveNorm;
      this.mvMatrix = saveMV;
      this.prmvMatrix = savePRMV;
        
      return result;
    };
    
    /**
     * Prepare clipplanes for drawing
     * @param { object } obj - clip planes object
     * @param { object } subscene
     */
    rglwidgetClass.prototype.drawClipplanes = function(obj) {
      var count = obj.offsets.length,
        IMVClip = [];
      for (var i=0; i < count; i++) {
        IMVClip[i] = this.multMV(this.invMatrix, obj.vClipplane.slice(4*i, 4*(i+1)));
      }
      obj.IMVClip = IMVClip;
      return [];
    };
    
    rglwidgetClass.prototype.drawLinestrip = function(obj, subscene, context) {
      var origIndices, i, j;
      if (this.opaquePass)
        return this.drawSimple(obj, subscene, context);
      origIndices = context.indices.slice();
      for (i=0; i < origIndices.length; i++) {
        j = origIndices[i];
        if (j < obj.centers.length - 1) {
          context.indices = [j, j+1];
          this.drawSimple(obj, subscene, context);
        }
      }
      context.indices = origIndices;
      return [];
    };
          
    /**
     * Draw a sprites object in a subscene
     * @param { object } obj - object to draw
     * @param { object } subscene
     * @param { object } context
     */
    rglwidgetClass.prototype.drawSprites = function(obj, subscene, context) {
      var flags = obj.flags,
          is_transparent = this.isSet(flags, this.f_is_transparent),
          sprites3d = this.isSet(flags, this.f_sprites_3d),
          i,j,
          origMV = new CanvasMatrix4( this.mvMatrix ),
          origPRMV = null,
          pos, radius, userMatrix,
          result = [];

      if (!sprites3d) {
        return this.drawSimple(obj, subscene, context);
      }
      
      if (!obj.initialized)
        this.initObj(obj.id);

      if (!obj.vertexCount)
        return result;
    
      is_transparent = is_transparent || obj.someHidden;
      
      var norigs = obj.vertices.length,
          savenorm = new CanvasMatrix4(this.normMatrix),
          iOrig;

      this.normMatrix = subscene.spriteNormmat;
      userMatrix = obj.userMatrix;
                   
      if (this.opaquePass) {
        context = context.slice();
        context.push(obj.id);
      } else
        norigs = 1;
          
      if (this.prmvMatrix !== null)
         origPRMV = new CanvasMatrix4( this.prmvMatrix );

      for (iOrig=0; iOrig < norigs; iOrig++) {
        if (this.opaquePass)
          j = iOrig;
        else
          j = context.subid;

        pos = this.multVM([].concat(obj.vertices[j]).concat(1.0),
                          origMV);
        radius = obj.radii.length > 1 ? obj.radii[j][0] : obj.radii[0][0];
        this.mvMatrix = new CanvasMatrix4(userMatrix);
        this.mvMatrix.scale(radius);
        this.mvMatrix.translate(pos[0]/pos[3], pos[1]/pos[3], pos[2]/pos[3]);
        this.setprmvMatrix();
        for (i=0; i < obj.objects.length; i++)
          if (this.opaquePass)
            result = result.concat(this.drawObjId(obj.objects[i], subscene.id, context.concat(j)));
          else
            this.drawObjId(obj.objects[i], subscene.id, context);
      }
      this.normMatrix = savenorm;
      this.mvMatrix = origMV;
      if (origPRMV !== null)
        this.prmvMatrix = origPRMV;
      return result;
    };
   
    rglwidgetClass.prototype.drawObjId = function(id, subsceneid, context) {
      if (typeof id !== "number")
        this.alertOnce("drawObjId id is "+typeof id);

      return this.drawObj(this.getObj(id), this.getObj(subsceneid), context);
   };
   
    /**
     * Draw an object in a subscene
     * @param { number } obj - object to draw
     * @param { number } subsceneid - id of subscene
     */
    rglwidgetClass.prototype.drawObj = function(obj, subscene, context) {
      switch(obj.type) {
        case "abclines":
        case "surface":
        case "triangles":
        case "quads":
        case "lines":
        case "points":
        case "text":
          return this.drawSimple(obj, subscene, context);
        case "linestrip":
          return this.drawLinestrip(obj, subscene, context);
        case "planes":
          return this.drawPlanes(obj, subscene, context);
        case "spheres":
          return this.drawSpheres(obj, subscene, context);
        case "clipplanes":
          return this.drawClipplanes(obj);
        case "sprites":
          return this.drawSprites(obj, subscene, context);
        case "light":
        case "bboxdeco":
          return [];
      }
      
      console.error("drawObj for type = "+obj.type);
    };

    /**
     * Draw the background for a subscene
     * @param { number } id - id of background object
     * @param { number } subsceneid - id of subscene
     */
    rglwidgetClass.prototype.drawBackground = function(id, subsceneid) {
      var gl = this.gl || this.initGL(),
          obj = this.getObj(id),
          bg, i, savepr, savemv;

      if (!obj.initialized)
        this.initObj(id);

      if (obj.colors.length) {
        bg = obj.colors[0];
        gl.clearColor(bg[0], bg[1], bg[2], bg[3]);
        gl.depthMask(true);
        /* jshint bitwise: false */
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        /* jshint bitwise: true */
        this.fogColor = bg;
      } else 
        this.fogColor = [0,0,0,0];
  
      this.fogType = obj.fogtype;
      this.fogScale = obj.fogscale;
      if (typeof obj.quad !== "undefined") {
        savepr = this.prMatrix;
        savemv = this.mvMatrix;
        this.prMatrix = new CanvasMatrix4();
        this.mvMatrix = new CanvasMatrix4();
        gl.disable(gl.BLEND);
        gl.disable(gl.DEPTH_TEST);
        gl.depthMask(false);
        for (i=0; i < obj.quad.length; i++)
          this.drawObjId(obj.quad[i], subsceneid);
        this.prMatrix = savepr;
        this.mvMatrix = savemv;
      }
    };

    /**
     * Draw a subscene
     * @param { number } subsceneid - id of subscene
     * @param { boolean } opaquePass - is this the opaque drawing pass?
     */
    rglwidgetClass.prototype.drawSubscene = function(subsceneid, context) {
      var sub = this.getObj(subsceneid),
          objects = this.scene.objects,
          clipids = sub.clipplanes,
          subids = sub.objects,
          subscene_has_faces = false,
          subscene_needs_sorting = false,
          flags, i, obj, result = [];
          
      if (sub.par3d.skipRedraw)
        return result;
      
      if (this.opaquePass) {
        for (i=0; i < subids.length; i++) {
      	  obj = objects[subids[i]];
          flags = obj.flags;
          if (typeof flags !== "undefined") {
            subscene_has_faces = subscene_has_faces || 
                            (this.isSet(flags, this.f_is_lit) &&
                            !this.isSet(flags, this.f_fixed_quads));
            obj.is_transparent = obj.someHidden || 
              this.isSet(flags, this.f_is_transparent);
            subscene_needs_sorting = subscene_needs_sorting || 
              obj.is_transparent ||
              this.isSet(flags, this.f_depth_sort);
          }
        }
      }

      this.setViewport(subsceneid);

      this.setprMatrix(subsceneid);
      this.setmvMatrix(subsceneid);
        
      if (typeof sub.backgroundId !== "undefined" && this.opaquePass)
        this.drawBackground(sub.backgroundId, subsceneid);

      if (subids.length) {
        if (subscene_has_faces) {
          this.setnormMatrix(subsceneid);
          if (this.isSet(sub.flags, this.f_sprites_3d) &&
              typeof sub.spriteNormmat === "undefined") {
            sub.spriteNormmat = new CanvasMatrix4(this.normMatrix);
          }
        }

        if (subscene_needs_sorting)
          this.setprmvMatrix();
            
        if (clipids.length > 0) {
          this.invMatrix = new CanvasMatrix4(this.mvMatrix);
          this.invMatrix.invert();
          for (i = 0; i < clipids.length; i++)
            this.drawObjId(clipids[i], subsceneid);
        }
        
        subids = sub.opaque.concat(sub.transparent);
        if (this.opaquePass) {
          context = context.slice();
          context.push(subsceneid);
        
          this.doBlending(false);
          this.subsceneid = subsceneid;
          if (typeof this.sphere !== "undefined") // reset this.sphere.fastpieces; it will be recreated if needed
            this.sphere.fastpieces = undefined;
          for (i = 0; i < subids.length; i++)
            result = result.concat(this.drawObjId(subids[i], subsceneid, context));
          subids = sub.subscenes;
          for (i = 0; i < subids.length; i++)
            result = result.concat(this.drawSubscene(subids[i], context));
        }
      }
      return result;
    };
    
    /**
     * Set the context for drawing transparently
     */
    rglwidgetClass.prototype.setContext = function(context) {
      var result = [], objid, obj, type;
      context = context.slice();
      context.reverse();
      while (context.length > 0) {
        objid = context.pop();
        obj = this.getObj(objid);
        type = obj.type;
        switch (type) {
          case "subscene":
            this.drawSubscene(objid, false);
            break;
          case "sprites":
            result = result.concat(context.pop());
            break;
          case "spheres":
            // this.initSphereFromObj(obj);  // FIXME:  not needed?
            break;
          default:
            console.error("bad type '", type, "' in setContext");
        }
      }
      return result;
    };
    
    /**
     * Draw the transparent pieces of a scene
     */
    rglwidgetClass.prototype.drawPieces = function(pieces) {
      var i, prevcontext = [], context;
      this.doBlending(true);
      for (i = 0; i < pieces.length; i++) {
        context = pieces[i].context.slice();
        if (context !== prevcontext) {
          prevcontext = context.slice();
          context = this.setContext(context);
        }
        this.drawObjId(pieces[i].objid, this.subsceneid, 
                       pieces[i]);
      }
    };
 
    /**
     * Draw the whole scene
     */
    rglwidgetClass.prototype.drawScene = function() {
      var wasDrawing = this.startDrawing(),
          pieces;
      if (!wasDrawing) {
        if (this.select.state !== "inactive")
          this.selectionChanged();

        this.doStartScene();
        this.opaquePass = true;
        pieces = this.drawSubscene(this.scene.rootSubscene, []);
        this.opaquePass = false;
        pieces = this.sortPieces(pieces);
        pieces = this.mergePieces(pieces);
        this.drawPieces(pieces);
      }
      this.stopDrawing(wasDrawing);
    };
