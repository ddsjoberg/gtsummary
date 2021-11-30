
    /**
     * Generate the vertex shader for an object
     * @returns {string}
     * @param { number } id - Id of object
     */
    rglwidgetClass.prototype.getVertexShader = function(id) {
      var obj = this.getObj(id),
          userShader = obj.userVertexShader,
          flags = obj.flags,
          type = obj.type,
          is_lit = this.isSet(flags, this.f_is_lit),
          has_texture = this.isSet(flags, this.f_has_texture),
          fixed_quads = this.isSet(flags, this.f_fixed_quads),
          sprites_3d = this.isSet(flags, this.f_sprites_3d),
          nclipplanes = this.countClipplanes(),
          fixed_size = this.isSet(flags, this.f_fixed_size),
          is_points = this.isSet(flags, this.f_is_points),
          is_twosided = this.isSet(flags, this.f_is_twosided),
          fat_lines = this.isSet(flags, this.f_fat_lines),
          is_brush = this.isSet(flags, this.f_is_brush),
          has_fog = this.isSet(flags, this.f_has_fog),
          has_normals = typeof obj.normals !== "undefined",
          needs_vnormal = (is_lit && !fixed_quads && !is_brush) || (is_twosided && (has_normals || obj.type === "spheres")),
          result;

      if (type === "clipplanes" || sprites_3d) return;

      if (typeof userShader !== "undefined") return userShader;

      result = "  /* ****** "+type+" object "+id+" vertex shader ****** */\n"+
      "#ifdef GL_ES\n"+
      "#ifdef GL_FRAGMENT_PRECISION_HIGH\n"+
      "  precision highp float;\n"+
      "#else\n"+
      "  precision mediump float;\n"+
      "#endif\n"+
      "#endif\n"+
      "  attribute vec3 aPos;\n"+
      "  attribute vec4 aCol;\n"+
      "  uniform mat4 mvMatrix;\n"+
      "  uniform mat4 prMatrix;\n"+
      "  varying vec4 vCol;\n"+
      "  varying vec4 vPosition;\n";

      if (needs_vnormal)
        result = result + "  attribute vec3 aNorm;\n"+
                          "  uniform mat4 normMatrix;\n"+
                          "  varying vec3 vNormal;\n";

      if (has_texture || type === "text")
        result = result + "  attribute vec2 aTexcoord;\n"+
                          "  varying vec2 vTexcoord;\n";

      if (fixed_size)
        result = result + "  uniform vec2 textScale;\n";

      if (fixed_quads)
        result = result + "  attribute vec2 aOfs;\n";

      if (is_twosided)
        if (has_normals || obj.type === "spheres")
          result = result + "  varying float normz;\n";
        else
          result = result + "  attribute vec3 aPos1;\n"+
                            "  attribute vec3 aPos2;\n"+
                            "  varying float normz;\n";

      if (fat_lines) {
      	result = result +   "  attribute vec3 aNext;\n"+
                            "  attribute vec2 aPoint;\n"+
                            "  varying vec2 vPoint;\n"+
                            "  varying float vLength;\n"+
                            "  uniform float uAspect;\n"+
                            "  uniform float uLwd;\n";
      }
      
      result = result + "  void main(void) {\n";

      if ((nclipplanes || !fixed_quads || has_fog) && !is_brush)
        result = result + "    vPosition = mvMatrix * vec4(aPos, 1.);\n";

      if (!fixed_quads && !is_brush)
        result = result + "    gl_Position = prMatrix * vPosition;\n";

      if (is_points) {
        var size = this.getMaterial(id, "size");
        result = result + "    gl_PointSize = "+size.toFixed(1)+";\n";
      }

      result = result + "    vCol = aCol;\n";

      if (needs_vnormal)
        result = result + "    vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);\n";

      if (has_texture || type === "text")
        result = result + "    vTexcoord = aTexcoord;\n";

      if (fixed_size)
        result = result + "    vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n"+
                          "   pos = pos/pos.w;\n"+
                          "   gl_Position = pos + vec4(aOfs*textScale, 0.,0.);\n";

      if (type === "sprites" && !fixed_size)
        result = result + "    vec4 pos = mvMatrix * vec4(aPos, 1.);\n"+
                          "   pos = pos/pos.w + vec4(aOfs, 0., 0.);\n"+
                          "   gl_Position = prMatrix*pos;\n";

      if (is_twosided)
        if (has_normals || obj.type === "spheres")
          result = result + "   normz = vNormal.z;";
        else
          result = result + "   vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n"+
                            "   pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n"+
                            "   vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n"+
                            "   pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n"+
                            "   normz = pos1.x*pos2.y - pos1.y*pos2.x;\n";
                          
      if (fat_lines) 
        /* This code was inspired by Matt Deslauriers' code in https://mattdesl.svbtle.com/drawing-lines-is-hard */
        result = result + "   vec2 aspectVec = vec2(uAspect, 1.0);\n"+
                          "   mat4 projViewModel = prMatrix * mvMatrix;\n"+
                          "   vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n"+
                          "   currentProjected = currentProjected/currentProjected.w;\n"+
                          "   vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n"+
                          "   vec2 currentScreen = currentProjected.xy * aspectVec;\n"+
                          "   vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n"+
                          "   float len = uLwd;\n"+
                          "   vec2 dir = vec2(1.0, 0.0);\n"+
                          "   vPoint = aPoint;\n"+
                          "   vLength = length(nextScreen - currentScreen)/2.0;\n"+
                          "   vLength = vLength/(vLength + len);\n"+
                          "   if (vLength > 0.0) {\n"+
                          "     dir = normalize(nextScreen - currentScreen);\n"+
                          "   }\n"+
                          "   vec2 normal = vec2(-dir.y, dir.x);\n"+
                          "   dir.x /= uAspect;\n"+
                          "   normal.x /= uAspect;\n"+
                          "   vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n"+
                          "   gl_Position = currentProjected + offset;\n";

      if (is_brush)
        result = result + "   gl_Position = vec4(aPos, 1.);\n";
        
      result = result + "  }\n";

      // console.log(result);
      return result;
    };

    /**
     * Generate the fragment shader for an object
     * @returns {string}
     * @param { number } id - Id of object
     */
    rglwidgetClass.prototype.getFragmentShader = function(id) {
      var obj = this.getObj(id),
          userShader = obj.userFragmentShader,
          flags = obj.flags,
          type = obj.type,
          is_lit = this.isSet(flags, this.f_is_lit),
          has_texture = this.isSet(flags, this.f_has_texture),
          fixed_quads = this.isSet(flags, this.f_fixed_quads),
          sprites_3d = this.isSet(flags, this.f_sprites_3d),
          is_twosided = this.isSet(flags, this.f_is_twosided),
          fat_lines = this.isSet(flags, this.f_fat_lines),
          is_transparent = this.isSet(flags, this.f_is_transparent),
          is_points = this.isSet(flags, this.f_is_points),
          has_fog = this.isSet(flags, this.f_has_fog),
          nclipplanes = this.countClipplanes(), i,
          texture_format, nlights,
          result;

      if (type === "clipplanes" || sprites_3d) return;

      if (typeof userShader !== "undefined") return userShader;

      if (has_texture)
        texture_format = this.getMaterial(id, "textype");

      result = "/* ****** "+type+" object "+id+" fragment shader ****** */\n"+
               "#ifdef GL_ES\n"+
               "#ifdef GL_FRAGMENT_PRECISION_HIGH\n"+
               "  precision highp float;\n"+
               "#else\n"+
               "  precision mediump float;\n"+
               "#endif\n"+
               "#endif\n"+
               "  varying vec4 vCol; // carries alpha\n"+
               "  varying vec4 vPosition;\n";

      if (has_texture || type === "text")
        result = result + "  varying vec2 vTexcoord;\n"+
                          "  uniform sampler2D uSampler;\n";

      if (has_fog)
        result = result + "  uniform int uFogMode;\n"+
                          "  uniform vec3 uFogColor;\n"+
                          "  uniform vec4 uFogParms;\n";

      if (is_lit && !fixed_quads)
        result = result + "  varying vec3 vNormal;\n";

      for (i = 0; i < nclipplanes; i++)
        result = result + "  uniform vec4 vClipplane"+i+";\n";

      if (is_lit) {
        nlights = this.countLights();
        if (nlights)
            result = result + "  uniform mat4 mvMatrix;\n";
        else
            is_lit = false;
      }

      if (is_lit) {
        result = result + "  uniform vec3 emission;\n"+
                          "  uniform float shininess;\n";

        for (i=0; i < nlights; i++) {
          result = result + "  uniform vec3 ambient" + i + ";\n"+
                            "  uniform vec3 specular" + i +"; // light*material\n"+
                            "  uniform vec3 diffuse" + i + ";\n"+
                            "  uniform vec3 lightDir" + i + ";\n"+
                            "  uniform bool viewpoint" + i + ";\n"+
                            "  uniform bool finite" + i + ";\n";
        }
      }

      if (is_twosided)
        result = result + "  uniform bool front;\n"+
                          "  varying float normz;\n";
                          
      if (fat_lines)
        result = result + "  varying vec2 vPoint;\n"+
                          "  varying float vLength;\n";

      result = result + "  void main(void) {\n"+
                        "    vec4 fragColor;\n";
      
      if (fat_lines) {
        result = result + "    vec2 point = vPoint;\n"+
                          "    bool neg = point.y < 0.0;\n"+
                          "    point.y = neg ? "+
                          "      (point.y + vLength)/(1.0 - vLength) :\n"+
                          "     -(point.y - vLength)/(1.0 - vLength);\n";
        if (is_transparent && type === "linestrip")
          result = result+"    if (neg && length(point) <= 1.0) discard;\n";
        result = result + "    point.y = min(point.y, 0.0);\n"+
                          "    if (length(point) > 1.0) discard;\n";
      }
      
      if (is_points) {
        var round = this.getMaterial(id, "point_antialias");
        if (round)
          result = result + "    vec2 coord = gl_PointCoord - vec2(0.5);\n"+
                            "    if (length(coord) > 0.5) discard;\n";
      }

      for (i=0; i < nclipplanes;i++)
        result = result + "    if (dot(vPosition, vClipplane"+i+") < 0.0) discard;\n";

      if (fixed_quads) {
        result = result +   "    vec3 n = vec3(0., 0., 1.);\n";
      } else if (is_lit) {
      	result = result +   "    vec3 n = normalize(vNormal);\n";
      }

      if (is_twosided) {
      	result = result +   "    if ((normz <= 0.) != front) discard;\n";
      }

      if (is_lit) {
        result = result + "    vec3 eye = normalize(-vPosition.xyz);\n"+
                          "   vec3 lightdir;\n"+
                          "   vec4 colDiff;\n"+
                          "   vec3 halfVec;\n"+
                          "   vec4 lighteffect = vec4(emission, 0.);\n"+
                          "   vec3 col;\n"+
                          "   float nDotL;\n";
        if (!fixed_quads) {
          result = result +   "   n = -faceforward(n, n, eye);\n";
        }
        for (i=0; i < nlights; i++) {
          result = result + "   colDiff = vec4(vCol.rgb * diffuse" + i + ", vCol.a);\n"+
                            "   lightdir = lightDir" + i + ";\n"+
                            "   if (!viewpoint" + i +")\n"+
                            "     lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n"+
                            "   if (!finite" + i + ") {\n"+
                            "     halfVec = normalize(lightdir + eye);\n"+
                            "   } else {\n"+
                            "     lightdir = normalize(lightdir - vPosition.xyz);\n"+
                            "     halfVec = normalize(lightdir + eye);\n"+
                            "   }\n"+
                            "    col = ambient" + i + ";\n"+
                            "   nDotL = dot(n, lightdir);\n"+
                            "   col = col + max(nDotL, 0.) * colDiff.rgb;\n"+
                            "   col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular" + i + ";\n"+
                            "   lighteffect = lighteffect + vec4(col, colDiff.a);\n";
        }

      } else {
        result = result +   "    vec4 colDiff = vCol;\n"+
                            "    vec4 lighteffect = colDiff;\n";
      }

      if (type === "text")
        result = result +   "    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n";

      if (has_texture) {
        result = result + {
            rgb:            "   vec4 textureColor = lighteffect*vec4(texture2D(uSampler, vTexcoord).rgb, 1.);\n",
            rgba:           "   vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n",
            alpha:          "   vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
                            "   float luminance = dot(vec3(1.,1.,1.), textureColor.rgb)/3.;\n"+
                            "   textureColor =  vec4(lighteffect.rgb, lighteffect.a*luminance);\n",
            luminance:      "   vec4 textureColor = vec4(lighteffect.rgb*dot(texture2D(uSampler, vTexcoord).rgb, vec3(1.,1.,1.))/3., lighteffect.a);\n",
          "luminance.alpha":"    vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
                            "    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n"+
                            "    textureColor = vec4(lighteffect.rgb*luminance, lighteffect.a*textureColor.a);\n"
          }[texture_format]+
                            "    fragColor = textureColor;\n";
      } else if (type === "text") {
        result = result +   "    if (textureColor.a < 0.1)\n"+
                            "      discard;\n"+
                            "    else\n"+
                            "      fragColor = textureColor;\n";
      } else
        result = result +   "    fragColor = lighteffect;\n";

      if (has_fog) {
        // uFogParms elements: x = near, y = far, z = fogscale, w = (1-sin(FOV/2))/(1+sin(FOV/2))
        // In Exp and Exp2: use density = density/far
        // fogF will be the proportion of fog
        // Initialize it to the linear value
        result = result +   "    float fogF;\n"+
                            "    if (uFogMode > 0) {\n"+
                            "      fogF = (uFogParms.y - vPosition.z/vPosition.w)/(uFogParms.y - uFogParms.x);\n"+
                            "      if (uFogMode > 1)\n"+
                            "        fogF = mix(uFogParms.w, 1.0, fogF);\n"+
                            "      fogF = fogF*uFogParms.z;\n"+
                            "      if (uFogMode == 2)\n"+
                            "        fogF = 1.0 - exp(-fogF);\n"+
  // Docs are wrong: use (density*c)^2, not density*c^2
  // https://gitlab.freedesktop.org/mesa/mesa/-/blob/master/src/mesa/swrast/s_fog.c#L58
                            "      else if (uFogMode == 3)\n"+
                            "        fogF = 1.0 - exp(-fogF*fogF);\n"+ 
                            "      fogF = clamp(fogF, 0.0, 1.0);\n"+
                            "      gl_FragColor = vec4(mix(fragColor.rgb, uFogColor, fogF), fragColor.a);\n"+
                          //  "      if (fogF < 0.) gl_FragColor = vec4(1.0,0.0,0.0,1.0); else if (fogF < 1.0) gl_FragColor = vec4(mix(fragColor.rgb, uFogColor, fogF), fragColor.a);else gl_FragColor = vec4(0.0,1.0,0.0,1.0);\n"+
                            "    } else gl_FragColor = fragColor;\n";
      } else
        result = result +   "    gl_FragColor = fragColor;\n";
 
      //if (fat_lines)
      //  result = result +   "   gl_FragColor = vec4(0.0, abs(point.x), abs(point.y), 1.0);"
      result = result +     "  }\n";

      // console.log(result);
      return result;
    };

    /**
     * Call gl functions to create and compile shader
     * @returns {Object}
     * @param { number } shaderType - gl code for shader type
     * @param { string } code - code for the shader
     */
    rglwidgetClass.prototype.getShader = function(shaderType, code) {
        var gl = this.gl, shader;
        shader = gl.createShader(shaderType);
        gl.shaderSource(shader, code);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS) && !gl.isContextLost())
            alert(gl.getShaderInfoLog(shader));
        return shader;
    };

