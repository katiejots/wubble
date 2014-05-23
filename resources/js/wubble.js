// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - e.target.offsetLeft, posy - e.target.offsetTop];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}

function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0/* () */ = 0, _1/* ++ */ = function(_2, _3){
  var _4 = E(_2);
  return _4[0]==0 ? E(_3) : [1, _4[1], new T(function(){
    return _1/* GHC.Base.++ */(_4[2], _3);
  })];
}, _5/* [] */ = [0], _6/* concurrent3 */ = function(_7, _){
  while(1){
    var _8 = E(_7);
    if(!_8[0]){
      return _0/* GHC.Tuple.() */;
    }else{
      var _9 = _8[2], _a = E(_8[1]);
      switch(_a[0]){
        case 0:
          var _b = A(_a[1],[_]);
          _7 = _1/* GHC.Base.++ */(_9, [1, _b, _5/* GHC.Types.[] */]);
          continue;
        case 1:
          _7 = _1/* GHC.Base.++ */(_9, _a[1]);
          continue;
        default:
          _7 = _9;
          continue;
      }
    }
  }
}, _c/* $fApplicativeCIO4 */ = function(_d, _e){
  return A(_e,[_d]);
}, _f/* $fMonadCIO1 */ = function(_g, _h, _i){
  return A(_g,[function(_j){
    return A(_h,[_j, _i]);
  }]);
}, _k/* a */ = function(_l, _m, _n){
  var _o = new T(function(){
    return A(_m,[_n]);
  });
  return A(_l,[function(_p){
    return E(_o);
  }]);
}, _q/* $fMonadCIO_$c>> */ = function(_r, _s, _t){
  return _k/* Haste.Concurrent.Monad.a */(_r, _s, _t);
}, _u/* $fMonadCIO_$cfail */ = function(_v){
  return err(_v);
}, _w/* $fMonadCIO */ = [0, _f/* Haste.Concurrent.Monad.$fMonadCIO1 */, _q/* Haste.Concurrent.Monad.$fMonadCIO_$c>> */, _c/* Haste.Concurrent.Monad.$fApplicativeCIO4 */, _u/* Haste.Concurrent.Monad.$fMonadCIO_$cfail */], _x/* $fMonadIOCIO1 */ = function(_y, _z){
  return [0, function(_){
    var _A = A(_y,[_]);
    return new T(function(){
      return A(_z,[_A]);
    });
  }];
}, _B/* $welemById */ = function(_C, _D){
  return A(_C,[function(_){
    return jsFind(toJSStr(E(_D)));
  }]);
}, _E/* any */ = function(_F, _G){
  while(1){
    var _H = E(_G);
    if(!_H[0]){
      return false;
    }else{
      if(!A(_F,[_H[1]])){
        _G = _H[2];
        continue;
      }else{
        return true;
      }
    }
  }
}, _I/* lvl */ = unCStr("Maybe.fromJust: Nothing"), _J/* fromJust1 */ = new T(function(){
  return err(_I/* Data.Maybe.lvl */);
}), _K/* fromJust */ = function(_L){
  var _M = E(_L);
  return _M[0]==0 ? E(_J/* Data.Maybe.fromJust1 */) : E(_M[1]);
}, _N/* isNothing */ = function(_O){
  return E(_O)[0]==0 ? true : false;
}, _P/* map */ = function(_Q, _R){
  var _S = E(_R);
  return _S[0]==0 ? [0] : [1, new T(function(){
    return A(_Q,[_S[1]]);
  }), new T(function(){
    return _P/* GHC.Base.map */(_Q, _S[2]);
  })];
}, _T/* $fShowChar1 */ = [0, 34], _U/* findElems */ = function(_V, _W){
  while(1){
    var _X = (function(_Y, _Z){
      var _10 = E(_Y);
      if(!_10[0]){
        return [0];
      }else{
        var _11 = _10[2], _12 = E(_Z);
        if(!_12[0]){
          return [0];
        }else{
          var _13 = _12[2];
          if(!E(_12[1])[0]){
            return [1, _10[1], new T(function(){
              return _U/* Haste.DOM.findElems */(_11, _13);
            })];
          }else{
            _V = _11;
            _W = _13;
            return null;
          }
        }
      }
    })(_V, _W);
    if(_X!=null){
      return _X;
    }
  }
}, _14/* lvl */ = new T(function(){
  return unAppCStr("[]", _5/* GHC.Types.[] */);
}), _15/* showList__3 */ = [0, 91], _16/* lvl3 */ = unCStr("Prelude.(!!): negative index\n"), _17/* !!1 */ = new T(function(){
  return err(_16/* GHC.List.lvl3 */);
}), _18/* lvl1 */ = unCStr("Prelude.(!!): index too large\n"), _19/* lvl2 */ = new T(function(){
  return err(_18/* GHC.List.lvl1 */);
}), _1a/* !!_sub */ = function(_1b, _1c){
  while(1){
    var _1d = E(_1b);
    if(!_1d[0]){
      return E(_19/* GHC.List.lvl2 */);
    }else{
      var _1e = E(_1c);
      if(!_1e){
        return E(_1d[1]);
      }else{
        _1b = _1d[2];
        _1c = _1e-1|0;
        continue;
      }
    }
  }
}, _1f/* asciiTab59 */ = unCStr("ACK"), _1g/* asciiTab58 */ = unCStr("BEL"), _1h/* asciiTab57 */ = unCStr("BS"), _1i/* asciiTab33 */ = unCStr("SP"), _1j/* asciiTab32 */ = [1, _1i/* GHC.Show.asciiTab33 */, _5/* GHC.Types.[] */], _1k/* asciiTab34 */ = unCStr("US"), _1l/* asciiTab31 */ = [1, _1k/* GHC.Show.asciiTab34 */, _1j/* GHC.Show.asciiTab32 */], _1m/* asciiTab35 */ = unCStr("RS"), _1n/* asciiTab30 */ = [1, _1m/* GHC.Show.asciiTab35 */, _1l/* GHC.Show.asciiTab31 */], _1o/* asciiTab36 */ = unCStr("GS"), _1p/* asciiTab29 */ = [1, _1o/* GHC.Show.asciiTab36 */, _1n/* GHC.Show.asciiTab30 */], _1q/* asciiTab37 */ = unCStr("FS"), _1r/* asciiTab28 */ = [1, _1q/* GHC.Show.asciiTab37 */, _1p/* GHC.Show.asciiTab29 */], _1s/* asciiTab38 */ = unCStr("ESC"), _1t/* asciiTab27 */ = [1, _1s/* GHC.Show.asciiTab38 */, _1r/* GHC.Show.asciiTab28 */], _1u/* asciiTab39 */ = unCStr("SUB"), _1v/* asciiTab26 */ = [1, _1u/* GHC.Show.asciiTab39 */, _1t/* GHC.Show.asciiTab27 */], _1w/* asciiTab40 */ = unCStr("EM"), _1x/* asciiTab25 */ = [1, _1w/* GHC.Show.asciiTab40 */, _1v/* GHC.Show.asciiTab26 */], _1y/* asciiTab41 */ = unCStr("CAN"), _1z/* asciiTab24 */ = [1, _1y/* GHC.Show.asciiTab41 */, _1x/* GHC.Show.asciiTab25 */], _1A/* asciiTab42 */ = unCStr("ETB"), _1B/* asciiTab23 */ = [1, _1A/* GHC.Show.asciiTab42 */, _1z/* GHC.Show.asciiTab24 */], _1C/* asciiTab43 */ = unCStr("SYN"), _1D/* asciiTab22 */ = [1, _1C/* GHC.Show.asciiTab43 */, _1B/* GHC.Show.asciiTab23 */], _1E/* asciiTab44 */ = unCStr("NAK"), _1F/* asciiTab21 */ = [1, _1E/* GHC.Show.asciiTab44 */, _1D/* GHC.Show.asciiTab22 */], _1G/* asciiTab45 */ = unCStr("DC4"), _1H/* asciiTab20 */ = [1, _1G/* GHC.Show.asciiTab45 */, _1F/* GHC.Show.asciiTab21 */], _1I/* asciiTab46 */ = unCStr("DC3"), _1J/* asciiTab19 */ = [1, _1I/* GHC.Show.asciiTab46 */, _1H/* GHC.Show.asciiTab20 */], _1K/* asciiTab47 */ = unCStr("DC2"), _1L/* asciiTab18 */ = [1, _1K/* GHC.Show.asciiTab47 */, _1J/* GHC.Show.asciiTab19 */], _1M/* asciiTab48 */ = unCStr("DC1"), _1N/* asciiTab17 */ = [1, _1M/* GHC.Show.asciiTab48 */, _1L/* GHC.Show.asciiTab18 */], _1O/* asciiTab49 */ = unCStr("DLE"), _1P/* asciiTab16 */ = [1, _1O/* GHC.Show.asciiTab49 */, _1N/* GHC.Show.asciiTab17 */], _1Q/* asciiTab50 */ = unCStr("SI"), _1R/* asciiTab15 */ = [1, _1Q/* GHC.Show.asciiTab50 */, _1P/* GHC.Show.asciiTab16 */], _1S/* asciiTab51 */ = unCStr("SO"), _1T/* asciiTab14 */ = [1, _1S/* GHC.Show.asciiTab51 */, _1R/* GHC.Show.asciiTab15 */], _1U/* asciiTab52 */ = unCStr("CR"), _1V/* asciiTab13 */ = [1, _1U/* GHC.Show.asciiTab52 */, _1T/* GHC.Show.asciiTab14 */], _1W/* asciiTab53 */ = unCStr("FF"), _1X/* asciiTab12 */ = [1, _1W/* GHC.Show.asciiTab53 */, _1V/* GHC.Show.asciiTab13 */], _1Y/* asciiTab54 */ = unCStr("VT"), _1Z/* asciiTab11 */ = [1, _1Y/* GHC.Show.asciiTab54 */, _1X/* GHC.Show.asciiTab12 */], _20/* asciiTab55 */ = unCStr("LF"), _21/* asciiTab10 */ = [1, _20/* GHC.Show.asciiTab55 */, _1Z/* GHC.Show.asciiTab11 */], _22/* asciiTab56 */ = unCStr("HT"), _23/* asciiTab9 */ = [1, _22/* GHC.Show.asciiTab56 */, _21/* GHC.Show.asciiTab10 */], _24/* asciiTab8 */ = [1, _1h/* GHC.Show.asciiTab57 */, _23/* GHC.Show.asciiTab9 */], _25/* asciiTab7 */ = [1, _1g/* GHC.Show.asciiTab58 */, _24/* GHC.Show.asciiTab8 */], _26/* asciiTab6 */ = [1, _1f/* GHC.Show.asciiTab59 */, _25/* GHC.Show.asciiTab7 */], _27/* asciiTab60 */ = unCStr("ENQ"), _28/* asciiTab5 */ = [1, _27/* GHC.Show.asciiTab60 */, _26/* GHC.Show.asciiTab6 */], _29/* asciiTab61 */ = unCStr("EOT"), _2a/* asciiTab4 */ = [1, _29/* GHC.Show.asciiTab61 */, _28/* GHC.Show.asciiTab5 */], _2b/* asciiTab62 */ = unCStr("ETX"), _2c/* asciiTab3 */ = [1, _2b/* GHC.Show.asciiTab62 */, _2a/* GHC.Show.asciiTab4 */], _2d/* asciiTab63 */ = unCStr("STX"), _2e/* asciiTab2 */ = [1, _2d/* GHC.Show.asciiTab63 */, _2c/* GHC.Show.asciiTab3 */], _2f/* asciiTab64 */ = unCStr("SOH"), _2g/* asciiTab1 */ = [1, _2f/* GHC.Show.asciiTab64 */, _2e/* GHC.Show.asciiTab2 */], _2h/* asciiTab65 */ = unCStr("NUL"), _2i/* asciiTab */ = [1, _2h/* GHC.Show.asciiTab65 */, _2g/* GHC.Show.asciiTab1 */], _2j/* lvl */ = [0, 92], _2k/* lvl1 */ = unCStr("\\DEL"), _2l/* lvl10 */ = unCStr("\\a"), _2m/* lvl2 */ = unCStr("\\\\"), _2n/* lvl3 */ = unCStr("\\SO"), _2o/* lvl4 */ = unCStr("\\r"), _2p/* lvl5 */ = unCStr("\\f"), _2q/* lvl6 */ = unCStr("\\v"), _2r/* lvl7 */ = unCStr("\\n"), _2s/* lvl8 */ = unCStr("\\t"), _2t/* lvl9 */ = unCStr("\\b"), _2u/* $wshowLitChar */ = function(_2v, _2w){
  if(_2v<=127){
    var _2x = E(_2v);
    switch(_2x){
      case 92:
        return _1/* GHC.Base.++ */(_2m/* GHC.Show.lvl2 */, _2w);
      case 127:
        return _1/* GHC.Base.++ */(_2k/* GHC.Show.lvl1 */, _2w);
      default:
        if(_2x<32){
          var _2y = E(_2x);
          switch(_2y){
            case 7:
              return _1/* GHC.Base.++ */(_2l/* GHC.Show.lvl10 */, _2w);
            case 8:
              return _1/* GHC.Base.++ */(_2t/* GHC.Show.lvl9 */, _2w);
            case 9:
              return _1/* GHC.Base.++ */(_2s/* GHC.Show.lvl8 */, _2w);
            case 10:
              return _1/* GHC.Base.++ */(_2r/* GHC.Show.lvl7 */, _2w);
            case 11:
              return _1/* GHC.Base.++ */(_2q/* GHC.Show.lvl6 */, _2w);
            case 12:
              return _1/* GHC.Base.++ */(_2p/* GHC.Show.lvl5 */, _2w);
            case 13:
              return _1/* GHC.Base.++ */(_2o/* GHC.Show.lvl4 */, _2w);
            case 14:
              return _1/* GHC.Base.++ */(_2n/* GHC.Show.lvl3 */, new T(function(){
                var _2z = E(_2w);
                return _2z[0]==0 ? [0] : E(E(_2z[1])[1])==72 ? unAppCStr("\\&", _2z) : E(_2z);
              }));
            default:
              return _1/* GHC.Base.++ */([1, _2j/* GHC.Show.lvl */, new T(function(){
                var _2A = _2y;
                return _2A>=0 ? _1a/* GHC.List.!!_sub */(_2i/* GHC.Show.asciiTab */, _2A) : E(_17/* GHC.List.!!1 */);
              })], _2w);
          }
        }else{
          return [1, [0, _2x], _2w];
        }
    }
  }else{
    return [1, _2j/* GHC.Show.lvl */, new T(function(){
      var _2B = jsShowI(_2v);
      return _1/* GHC.Base.++ */(fromJSStr(_2B), new T(function(){
        var _2C = E(_2w);
        if(!_2C[0]){
          return [0];
        }else{
          var _2D = E(_2C[1])[1];
          return _2D<48 ? E(_2C) : _2D>57 ? E(_2C) : unAppCStr("\\&", _2C);
        }
      }));
    })];
  }
}, _2E/* lvl11 */ = unCStr("\\\""), _2F/* showLitString */ = function(_2G, _2H){
  var _2I = E(_2G);
  if(!_2I[0]){
    return E(_2H);
  }else{
    var _2J = _2I[2], _2K = E(E(_2I[1])[1]);
    return _2K==34 ? _1/* GHC.Base.++ */(_2E/* GHC.Show.lvl11 */, new T(function(){
      return _2F/* GHC.Show.showLitString */(_2J, _2H);
    })) : _2u/* GHC.Show.$wshowLitChar */(_2K, new T(function(){
      return _2F/* GHC.Show.showLitString */(_2J, _2H);
    }));
  }
}, _2L/* showList__2 */ = [0, 93], _2M/* lvl16 */ = [1, _2L/* GHC.Show.showList__2 */, _5/* GHC.Types.[] */], _2N/* showList__1 */ = [0, 44], _2O/* showl */ = function(_2P){
  var _2Q = E(_2P);
  return _2Q[0]==0 ? E(_2M/* Haste.DOM.lvl16 */) : [1, _2N/* GHC.Show.showList__1 */, [1, _T/* GHC.Show.$fShowChar1 */, new T(function(){
    return _2F/* GHC.Show.showLitString */(_2Q[1], [1, _T/* GHC.Show.$fShowChar1 */, new T(function(){
      return _2O/* Haste.DOM.showl */(_2Q[2]);
    })]);
  })]];
}, _2R/* withElems1 */ = function(_2S, _2T){
  return err(unAppCStr("Elements with the following IDs could not be found: ", new T(function(){
    var _2U = _U/* Haste.DOM.findElems */(_2T, _2S);
    return _2U[0]==0 ? E(_14/* Haste.DOM.lvl */) : [1, _15/* GHC.Show.showList__3 */, [1, _T/* GHC.Show.$fShowChar1 */, new T(function(){
      return _2F/* GHC.Show.showLitString */(_2U[1], [1, _T/* GHC.Show.$fShowChar1 */, new T(function(){
        return _2O/* Haste.DOM.showl */(_2U[2]);
      })]);
    })]];
  })));
}, _2V/* $wwithElems */ = function(_2W, _2X, _2Y, _2Z){
  var _30 = E(_2W), _31 = _30[1], _32 = _30[3];
  return A(_31,[new T(function(){
    var _33 = new T(function(){
      return A(_32,[_5/* GHC.Types.[] */]);
    }), _34 = function(_35){
      var _36 = E(_35);
      if(!_36[0]){
        return E(_33);
      }else{
        var _37 = new T(function(){
          return _34(_36[2]);
        });
        return A(_31,[new T(function(){
          return _B/* Haste.DOM.$welemById */(_2X, _36[1]);
        }), function(_38){
          return A(_31,[_37, function(_39){
            return A(_32,[[1, _38, _39]]);
          }]);
        }]);
      }
    };
    return _34(_2Y);
  }), function(_3a){
    return !_E/* GHC.List.any */(_N/* Data.Maybe.isNothing */, _3a) ? A(_2Z,[new T(function(){
      return _P/* GHC.Base.map */(_K/* Data.Maybe.fromJust */, _3a);
    })]) : _2R/* Haste.DOM.withElems1 */(_3a, _2Y);
  }]);
}, _3b/* !1 */ = function(_3c){
  return err(unAppCStr("Haste.JSON.!: unable to look up key ", new T(function(){
    return fromJSStr(E(_3c)[1]);
  })));
}, _3d/* $fEqPtr_$c/= */ = function(_3e, _3f){
  var _3g = strEq(E(_3e)[1], E(_3f)[1]);
  return E(_3g)==0 ? true : false;
}, _3h/* strEq */ = function(_3i, _3j){
  var _3k = strEq(E(_3i)[1], E(_3j)[1]);
  return E(_3k)==0 ? false : true;
}, _3l/* $fEqPtr */ = [0, _3h/* Haste.Prim.strEq */, _3d/* Haste.Prim.$fEqPtr_$c/= */], _3m/* == */ = function(_3n){
  return E(E(_3n)[1]);
}, _3o/* lookup */ = function(_3p, _3q, _3r){
  while(1){
    var _3s = E(_3r);
    if(!_3s[0]){
      return [0];
    }else{
      var _3t = E(_3s[1]);
      if(!A(_3m/* GHC.Classes.== */,[_3p, _3q, _3t[1]])){
        _3r = _3s[2];
        continue;
      }else{
        return [1, _3t[2]];
      }
    }
  }
}, _3u/* ! */ = function(_3v, _3w){
  var _3x = E(_3v);
  if(_3x[0]==4){
    var _3y = _3o/* GHC.List.lookup */(_3l/* Haste.Prim.$fEqPtr */, _3w, _3x[1]);
    return _3y[0]==0 ? _3b/* Haste.JSON.!1 */(_3w) : E(_3y[1]);
  }else{
    return _3b/* Haste.JSON.!1 */(_3w);
  }
}, _3z/* fromJSStr */ = function(_3A){
  return fromJSStr(E(_3A)[1]);
}, _3B/* getDefAtIndex2 */ = unCStr("meaning"), _3C/* getDefAtIndex1 */ = new T(function(){
  return [0, toJSStr(E(_3B/* Main.getDefAtIndex2 */))];
}), _3D/* $fExceptionNestedAtomically3 */ = unCStr("Control.Exception.Base"), _3E/* $fExceptionNestedAtomically4 */ = unCStr("base"), _3F/* $fExceptionPatternMatchFail2 */ = unCStr("PatternMatchFail"), _3G/* $fExceptionPatternMatchFail1 */ = new T(function(){
  var _3H = hs_wordToWord64(18445595), _3I = hs_wordToWord64(52003073);
  return [0, _3H, _3I, [0, _3H, _3I, _3E/* Control.Exception.Base.$fExceptionNestedAtomically4 */, _3D/* Control.Exception.Base.$fExceptionNestedAtomically3 */, _3F/* Control.Exception.Base.$fExceptionPatternMatchFail2 */], _5/* GHC.Types.[] */];
}), _3J/* $fExceptionPatternMatchFail_$ctypeOf */ = function(_3K){
  return E(_3G/* Control.Exception.Base.$fExceptionPatternMatchFail1 */);
}, _3L/* $p1Exception */ = function(_3M){
  return E(E(_3M)[1]);
}, _3N/* cast */ = function(_3O, _3P, _3Q){
  var _3R = new T(function(){
    var _3S = A(_3O,[_3Q]), _3T = A(_3P,[new T(function(){
      var _3U = E(_3R);
      return _3U[0]==0 ? E(_J/* Data.Maybe.fromJust1 */) : E(_3U[1]);
    })]), _3V = hs_eqWord64(_3S[1], _3T[1]);
    if(!E(_3V)){
      return [0];
    }else{
      var _3W = hs_eqWord64(_3S[2], _3T[2]);
      return E(_3W)==0 ? [0] : [1, _3Q];
    }
  });
  return E(_3R);
}, _3X/* $fExceptionPatternMatchFail_$cfromException */ = function(_3Y){
  var _3Z = E(_3Y);
  return _3N/* Data.Typeable.cast */(_3L/* GHC.Exception.$p1Exception */(_3Z[1]), _3J/* Control.Exception.Base.$fExceptionPatternMatchFail_$ctypeOf */, _3Z[2]);
}, _40/* $fShowPatternMatchFail_$cshow */ = function(_41){
  return E(E(_41)[1]);
}, _42/* $fShowPatternMatchFail1 */ = function(_43, _44){
  return _1/* GHC.Base.++ */(E(_43)[1], _44);
}, _45/* showList__ */ = function(_46, _47, _48){
  var _49 = E(_47);
  return _49[0]==0 ? unAppCStr("[]", _48) : [1, _15/* GHC.Show.showList__3 */, new T(function(){
    return A(_46,[_49[1], new T(function(){
      var _4a = function(_4b){
        var _4c = E(_4b);
        return _4c[0]==0 ? E([1, _2L/* GHC.Show.showList__2 */, _48]) : [1, _2N/* GHC.Show.showList__1 */, new T(function(){
          return A(_46,[_4c[1], new T(function(){
            return _4a(_4c[2]);
          })]);
        })];
      };
      return _4a(_49[2]);
    })]);
  })];
}, _4d/* $fShowPatternMatchFail_$cshowList */ = function(_4e, _4f){
  return _45/* GHC.Show.showList__ */(_42/* Control.Exception.Base.$fShowPatternMatchFail1 */, _4e, _4f);
}, _4g/* $fShowPatternMatchFail_$cshowsPrec */ = function(_4h, _4i, _4j){
  return _1/* GHC.Base.++ */(E(_4i)[1], _4j);
}, _4k/* $fShowPatternMatchFail */ = [0, _4g/* Control.Exception.Base.$fShowPatternMatchFail_$cshowsPrec */, _40/* Control.Exception.Base.$fShowPatternMatchFail_$cshow */, _4d/* Control.Exception.Base.$fShowPatternMatchFail_$cshowList */], _4l/* $fExceptionPatternMatchFail */ = [0, _3J/* Control.Exception.Base.$fExceptionPatternMatchFail_$ctypeOf */, _4k/* Control.Exception.Base.$fShowPatternMatchFail */, _4m/* Control.Exception.Base.$fExceptionPatternMatchFail_$ctoException */, _3X/* Control.Exception.Base.$fExceptionPatternMatchFail_$cfromException */], _4m/* $fExceptionPatternMatchFail_$ctoException */ = function(_4n){
  return [0, _4l/* Control.Exception.Base.$fExceptionPatternMatchFail */, _4n];
}, _4o/* lvl1 */ = unCStr("Non-exhaustive patterns in"), _4p/* throw2 */ = function(_4q, _4r){
  return die(new T(function(){
    return A(_4r,[_4q]);
  }));
}, _4s/* $wspan */ = function(_4t, _4u){
  var _4v = E(_4u);
  if(!_4v[0]){
    return [0, _5/* GHC.Types.[] */, _5/* GHC.Types.[] */];
  }else{
    var _4w = _4v[1];
    if(!A(_4t,[_4w])){
      return [0, _5/* GHC.Types.[] */, _4v];
    }else{
      var _4x = new T(function(){
        var _4y = _4s/* GHC.List.$wspan */(_4t, _4v[2]);
        return [0, _4y[1], _4y[2]];
      });
      return [0, [1, _4w, new T(function(){
        return E(E(_4x)[1]);
      })], new T(function(){
        return E(E(_4x)[2]);
      })];
    }
  }
}, _4z/* untangle1 */ = [0, 32], _4A/* untangle3 */ = [0, 10], _4B/* untangle2 */ = [1, _4A/* GHC.IO.Exception.untangle3 */, _5/* GHC.Types.[] */], _4C/* untangle4 */ = function(_4D){
  return E(E(_4D)[1])==124 ? false : true;
}, _4E/* untangle */ = function(_4F, _4G){
  var _4H = _4s/* GHC.List.$wspan */(_4C/* GHC.IO.Exception.untangle4 */, unCStr(_4F)), _4I = _4H[1], _4J = function(_4K, _4L){
    return _1/* GHC.Base.++ */(_4K, new T(function(){
      return unAppCStr(": ", new T(function(){
        return _1/* GHC.Base.++ */(_4G, new T(function(){
          return _1/* GHC.Base.++ */(_4L, _4B/* GHC.IO.Exception.untangle2 */);
        }));
      }));
    }));
  }, _4M = E(_4H[2]);
  return _4M[0]==0 ? _4J(_4I, _5/* GHC.Types.[] */) : E(E(_4M[1])[1])==124 ? _4J(_4I, [1, _4z/* GHC.IO.Exception.untangle1 */, _4M[2]]) : _4J(_4I, _5/* GHC.Types.[] */);
}, _4N/* patError */ = function(_4O){
  return _4p/* GHC.Exception.throw2 */([0, new T(function(){
    return _4E/* GHC.IO.Exception.untangle */(_4O, _4o/* Control.Exception.Base.lvl1 */);
  })], _4m/* Control.Exception.Base.$fExceptionPatternMatchFail_$ctoException */);
}, _4P/* getDefAtIndex3 */ = new T(function(){
  return _4N/* Control.Exception.Base.patError */("wubble.hs:44:9-44|function extractStr");
}), _4Q/* getDefAtIndex5 */ = unCStr("word"), _4R/* getDefAtIndex4 */ = new T(function(){
  return [0, toJSStr(E(_4Q/* Main.getDefAtIndex5 */))];
}), _4S/* $wgetDefAtIndex */ = function(_4T, _4U){
  var _4V = new T(function(){
    var _4W = E(_4U)[1];
    return _4W>=0 ? _1a/* GHC.List.!!_sub */(_4T, _4W) : E(_17/* GHC.List.!!1 */);
  });
  return [0, new T(function(){
    var _4X = _3u/* Haste.JSON.! */(_4V, _4R/* Main.getDefAtIndex4 */);
    return _4X[0]==1 ? _3z/* GHC.HastePrim.fromJSStr */(_4X[1]) : E(_4P/* Main.getDefAtIndex3 */);
  }), new T(function(){
    var _4Y = _3u/* Haste.JSON.! */(_4V, _3C/* Main.getDefAtIndex1 */);
    return _4Y[0]==1 ? _3z/* GHC.HastePrim.fromJSStr */(_4Y[1]) : E(_4P/* Main.getDefAtIndex3 */);
  })];
}, _4Z/* $wlen */ = function(_50, _51){
  while(1){
    var _52 = E(_50);
    if(!_52[0]){
      return E(_51);
    }else{
      _50 = _52[2];
      var _53 = _51+1|0;
      _51 = _53;
      continue;
    }
  }
}, _54/* $wsetProp */ = function(_55, _56, _57, _58){
  return A(_55,[function(_){
    var _59 = jsSet(E(_56)[1], toJSStr(E(_57)), toJSStr(E(_58)));
    return _0/* GHC.Tuple.() */;
  }]);
}, _5a/* decodeJSON2 */ = unCStr("Invalid JSON!"), _5b/* decodeJSON1 */ = [0, _5a/* Haste.JSON.decodeJSON2 */], _5c/* innerHtml */ = unCStr("innerHTML"), _5d/* getInitData_$sgetInitData */ = function(_5e){
  var _5f = new T(function(){
    return _54/* Haste.DOM.$wsetProp */(_x/* Haste.Concurrent.Monad.$fMonadIOCIO1 */, _5e, _5c/* Main.innerHtml */, _5/* GHC.Types.[] */);
  });
  return function(_5g){
    return [0, function(_){
      var _5h = jsGet(E(_5e)[1], toJSStr(E(_5c/* Main.innerHtml */)));
      return new T(function(){
        var _5i = new T(function(){
          return A(_5g,[new T(function(){
            var _5j = jsParseJSON(toJSStr(fromJSStr(_5h))), _5k = E(_5j);
            return _5k[0]==0 ? E(_5b/* Haste.JSON.decodeJSON1 */) : [1, _5k[1]];
          })]);
        });
        return A(_5f,[function(_5l){
          return E(_5i);
        }]);
      });
    }];
  };
}, _5m/* $fEq[]_$c==1 */ = function(_5n, _5o){
  while(1){
    var _5p = E(_5n);
    if(!_5p[0]){
      return E(_5o)[0]==0 ? true : false;
    }else{
      var _5q = E(_5o);
      if(!_5q[0]){
        return false;
      }else{
        if(E(_5p[1])[1]!=E(_5q[1])[1]){
          return false;
        }else{
          _5n = _5p[2];
          _5o = _5q[2];
          continue;
        }
      }
    }
  }
}, _5r/* Nothing */ = [0], _5s/* handlePop1 */ = unCStr("You win! Refresh to play again."), _5t/* handlePop2 */ = unCStr("Game over! Refresh to try again."), _5u/* $w$shandlePop */ = function(_5v, _5w, _5x, _5y, _5z){
  if(!_5m/* GHC.Classes.$fEq[]_$c==1 */(_4S/* Main.$wgetDefAtIndex */(_5w, _5x)[1], _5z)){
    var _5A = new T(function(){
      return _54/* Haste.DOM.$wsetProp */(_x/* Haste.Concurrent.Monad.$fMonadIOCIO1 */, _5v, _5c/* Main.innerHtml */, _5t/* Main.handlePop2 */);
    });
    return function(_5B){
      var _5C = new T(function(){
        return A(_5B,[_5r/* Data.Maybe.Nothing */]);
      });
      return A(_5A,[function(_5D){
        return E(_5C);
      }]);
    };
  }else{
    var _5E = E(_5y), _5F = E(_5x)[1];
    if(_5E[1]>(_5F+1|0)){
      var _5G = [0, _5F+1|0], _5H = new T(function(){
        return _54/* Haste.DOM.$wsetProp */(_x/* Haste.Concurrent.Monad.$fMonadIOCIO1 */, _5v, _5c/* Main.innerHtml */, new T(function(){
          return E(_4S/* Main.$wgetDefAtIndex */(_5w, _5G)[2]);
        }));
      });
      return function(_5I){
        var _5J = new T(function(){
          return A(_5I,[[1, [0, _5w, _5G, _5E]]]);
        });
        return A(_5H,[function(_5K){
          return E(_5J);
        }]);
      };
    }else{
      var _5L = new T(function(){
        return _54/* Haste.DOM.$wsetProp */(_x/* Haste.Concurrent.Monad.$fMonadIOCIO1 */, _5v, _5c/* Main.innerHtml */, _5s/* Main.handlePop1 */);
      });
      return function(_5M){
        var _5N = new T(function(){
          return A(_5M,[_5r/* Data.Maybe.Nothing */]);
        });
        return A(_5L,[function(_5O){
          return E(_5N);
        }]);
      };
    }
  }
}, _5P/* handlePop_$shandlePop */ = function(_5Q, _5R, _5S){
  var _5T = E(_5R);
  return _5u/* Main.$w$shandlePop */(_5Q, _5T[1], _5T[2], _5T[3], _5S);
}, _5U/* loadGame1 */ = [0, 0], _5V/* lvl */ = unCStr("Pattern match failure in do expression at wubble.hs:18:5-25"), _5W/* loadGame2 */ = new T(function(){
  return _u/* Haste.Concurrent.Monad.$fMonadCIO_$cfail */(_5V/* Main.lvl */);
}), _5X/* loadGame3 */ = new T(function(){
  return _4N/* Control.Exception.Base.patError */("wubble.hs:(17,1)-(21,72)|function loadGame");
}), _5Y/* concurrent2 */ = function(_5Z){
  return [2];
}, _60/* forkIO1 */ = function(_t){
  return _5Y/* Haste.Concurrent.Monad.concurrent2 */(_t);
}, _61/* Stop */ = [2], _62/* modifyMVarIO4 */ = [1, _5/* GHC.Types.[] */], _63/* modifyMVarIO3 */ = function(_64, _65){
  return [0, function(_){
    var _66 = E(_64)[1], _67 = rMV(_66), _68 = E(_67);
    if(!_68[0]){
      var _69 = _68[1], _6a = E(_68[2]);
      if(!_6a[0]){
        var _ = wMV(_66, _62/* Haste.Concurrent.Monad.modifyMVarIO4 */);
        return new T(function(){
          return A(_65,[_69]);
        });
      }else{
        var _6b = E(_6a[1]), _ = wMV(_66, [0, _6b[1], _6a[2]]);
        return [1, [1, new T(function(){
          return A(_65,[_69]);
        }), [1, new T(function(){
          return A(_6b[2],[_60/* Haste.Concurrent.Monad.forkIO1 */]);
        }), _5/* GHC.Types.[] */]]];
      }
    }else{
      var _ = wMV(_66, [1, new T(function(){
        return _1/* GHC.Base.++ */(_68[1], [1, function(_6c){
          var _6d = new T(function(){
            return A(_65,[_6c]);
          });
          return function(_6e){
            return E(_6d);
          };
        }, _5/* GHC.Types.[] */]);
      })]);
      return _61/* Haste.Concurrent.Monad.Stop */;
    }
  }];
}, _6f/* server1 */ = function(_6g, _6h, _6i){
  var _6j = function(_6k, _6l){
    var _6m = new T(function(){
      return A(_6h,[_6l]);
    });
    return function(_6n){
      var _6o = new T(function(){
        return A(_6n,[_0/* GHC.Tuple.() */]);
      });
      return _63/* Haste.Concurrent.Monad.modifyMVarIO3 */(_6k, function(_6p){
        return A(_6m,[_6p, function(_6q){
          var _6r = E(_6q);
          return _6r[0]==0 ? E(_6o) : A(_6j,[_6k, _6r[1], _6n]);
        }]);
      });
    };
  };
  return [0, function(_){
    var _6s = nMV(_62/* Haste.Concurrent.Monad.modifyMVarIO4 */), _6t = [0, _6s];
    return [1, [1, new T(function(){
      return A(_6i,[_6t]);
    }), [1, new T(function(){
      return A(_6j,[_6t, _6g, _60/* Haste.Concurrent.Monad.forkIO1 */]);
    }), _5/* GHC.Types.[] */]]];
  }];
}, _6u/* loadGame */ = function(_6v){
  var _6w = E(_6v);
  if(!_6w[0]){
    return E(_5X/* Main.loadGame3 */);
  }else{
    var _6x = _6w[1], _6y = E(_6w[2]);
    if(!_6y[0]){
      return E(_5X/* Main.loadGame3 */);
    }else{
      if(!E(_6y[2])[0]){
        var _6z = new T(function(){
          return _5d/* Main.getInitData_$sgetInitData */(_6y[1]);
        });
        return function(_6A){
          return A(_6z,[function(_6B){
            var _6C = E(_6B);
            if(!_6C[0]){
              return E(_5W/* Main.loadGame2 */);
            }else{
              var _6D = E(_6C[1]);
              if(_6D[0]==3){
                var _6E = _6D[1], _6F = new T(function(){
                  return _6f/* Haste.Concurrent.server1 */([0, _6E, _5U/* Main.loadGame1 */, new T(function(){
                    return [0, _4Z/* GHC.List.$wlen */(_6E, 0)];
                  })], function(_6G, _6H){
                    return _5P/* Main.handlePop_$shandlePop */(_6x, _6G, _6H);
                  }, _6A);
                });
                return A(_54/* Haste.DOM.$wsetProp */,[_x/* Haste.Concurrent.Monad.$fMonadIOCIO1 */, _6x, _5c/* Main.innerHtml */, new T(function(){
                  return E(_4S/* Main.$wgetDefAtIndex */(_6E, _5U/* Main.loadGame1 */)[2]);
                }), function(_6I){
                  return E(_6F);
                }]);
              }else{
                return E(_5W/* Main.loadGame2 */);
              }
            }
          }]);
        };
      }else{
        return E(_5X/* Main.loadGame3 */);
      }
    }
  }
}, _6J/* modifyMVarIO2 */ = function(_6K, _6L, _6M){
  return [0, function(_){
    var _6N = E(_6K)[1], _6O = rMV(_6N), _6P = E(_6O);
    if(!_6P[0]){
      var _ = wMV(_6N, [0, _6P[1], new T(function(){
        var _6Q = new T(function(){
          return A(_6M,[_0/* GHC.Tuple.() */]);
        });
        return _1/* GHC.Base.++ */(_6P[2], [1, [0, _6L, function(_6R){
          return E(_6Q);
        }], _5/* GHC.Types.[] */]);
      })]);
      return _61/* Haste.Concurrent.Monad.Stop */;
    }else{
      var _6S = E(_6P[1]);
      if(!_6S[0]){
        var _ = wMV(_6N, [0, _6L, _5/* GHC.Types.[] */]);
        return new T(function(){
          return A(_6M,[_0/* GHC.Tuple.() */]);
        });
      }else{
        var _ = wMV(_6N, [1, _6S[2]]);
        return [1, [1, new T(function(){
          return A(_6M,[_0/* GHC.Tuple.() */]);
        }), [1, new T(function(){
          return A(_6S[1],[_6L, _60/* Haste.Concurrent.Monad.forkIO1 */]);
        }), _5/* GHC.Types.[] */]]];
      }
    }
  }];
}, _6T/* addHandlers2 */ = function(_6U, _6V, _6W, _6X){
  return [0, function(_){
    var _6Y = E(_6V)[1], _6Z = jsGet(_6Y, toJSStr(E(_5c/* Main.innerHtml */)));
    return [0, function(_){
      var _70 = jsKillChild(_6Y, E(_6U)[1]);
      return new T(function(){
        return _6J/* Haste.Concurrent.Monad.modifyMVarIO2 */(_6W, new T(function(){
          return fromJSStr(_6Z);
        }), _6X);
      });
    }];
  }];
}, _71/* evtName7 */ = [0, "click"], _72/* addHandlers1 */ = function(_73, _74, _75){
  return [0, function(_){
    var _76 = E(_74), _77 = jsGetChildren(_76[1]);
    return new T(function(){
      var _78 = function(_79, _7a){
        var _7b = E(_79);
        if(!_7b[0]){
          return A(_7a,[_0/* GHC.Tuple.() */]);
        }else{
          var _7c = _7b[1], _7d = new T(function(){
            return _6T/* Main.addHandlers2 */(_76, _7c, _73, _5Y/* Haste.Concurrent.Monad.concurrent2 */);
          });
          return [0, function(_){
            var _7e = jsSetCB(E(_7c)[1], E(_71/* Haste.Callback.evtName7 */)[1], function(_7f, _7g, _){
              return _6/* Haste.Concurrent.Monad.concurrent3 */([1, _7d, _5/* GHC.Types.[] */], _);
            });
            return new T(function(){
              return _78(_7b[2], _7a);
            });
          }];
        }
      };
      return _78(_77, _75);
    });
  }];
}, _7h/* main5 */ = unCStr("bubbles"), _7i/* lvl1 */ = unCStr(" could be found!"), _7j/* withElem1 */ = function(_7k){
  return err(unAppCStr("No element with ID ", new T(function(){
    return _1/* GHC.Base.++ */(_7k, _7i/* Haste.DOM.lvl1 */);
  })));
}, _7l/* main4 */ = function(_7m){
  return [0, function(_){
    var _7n = E(_7h/* Main.main5 */), _7o = jsFind(toJSStr(_7n));
    return new T(function(){
      var _7p = E(_7o);
      return _7p[0]==0 ? _7j/* Haste.DOM.withElem1 */(_7n) : _72/* Main.addHandlers1 */(_7m, _7p[1], _5Y/* Haste.Concurrent.Monad.concurrent2 */);
    });
  }];
}, _7q/* main8 */ = unCStr("gamedata"), _7r/* main7 */ = [1, _7q/* Main.main8 */, _5/* GHC.Types.[] */], _7s/* main9 */ = unCStr("gametext"), _7t/* main6 */ = [1, _7s/* Main.main9 */, _7r/* Main.main7 */], _7u/* main3 */ = new T(function(){
  return A(_2V/* Haste.DOM.$wwithElems */,[_w/* Haste.Concurrent.Monad.$fMonadCIO */, _x/* Haste.Concurrent.Monad.$fMonadIOCIO1 */, _7t/* Main.main6 */, _6u/* Main.loadGame */, _7l/* Main.main4 */]);
}), _7v/* main2 */ = [1, _7u/* Main.main3 */, _5/* GHC.Types.[] */], _7w/* main1 */ = function(_){
  return _6/* Haste.Concurrent.Monad.concurrent3 */(_7v/* Main.main2 */, _);
}, _7x/* main */ = function(_){
  return _7w/* Main.main1 */(_);
};

var hasteMain = function() {A(_7x, [0]);};window.onload = hasteMain;