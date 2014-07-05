/**
 * @param {Element} el
 * @param {Object=} opts
 * @return {Hammer.Instance}
 */
var Hammer = function(el, opts) {};

/**
 * @type {Object{behavior:Object}
 */
Hammer.defaults;


/**
 * @class
 */
Hammer.Instance;

/**
 * @param {string} e
 * @param {Function} f
 */
Hammer.Instance.prototype.on = function(e, f) {};

/**
 * @param {string} e
 * @param {Function} f
 */
Hammer.Instance.prototype.off = function(e, f) {};

/**
 * @param {string} e
 * @param {Object} data
 */
Hammer.Instance.prototype.trigger = function(e, data) {};

Hammer.Instance.prototype.dispose = function() {};

/**
 * @type {Object{gesture:Object}}
 */
Hammer.Event;

/*
timestamp        {Number}        time the event occurred
target           {HTMLElement}   target element
touches          {Array}         touches (fingers, mouse) on the screen
pointerType      {String}        kind of pointer that was used. matches Hammer.POINTER_MOUSE|TOUCH
center           {Object}        center position of the touches. contains pageX and pageY
deltaTime        {Number}        the total time of the touches in the screen
deltaX           {Number}        the delta on x axis we haved moved
deltaY           {Number}        the delta on y axis we haved moved
velocityX        {Number}        the velocity on the x
velocityY        {Number}        the velocity on y
angle            {Number}        the angle we are moving from the start point.
interimAngle     {Number}        interim angle since the last movement.
direction        {String}        the direction moving from the start point. matches Hammer.DIRECTION_UP|DOWN|LEFT|RIGHT
interimDirection {String}        interim direction since the last movement. matches Hammer.DIRECTION_UP|DOWN|LEFT|RIGHT
distance         {Number}        the distance we haved moved
scale            {Number}        scaling of the touches, needs 2 touches
rotation         {Number}        rotation of the touches, needs 2 touches *
eventType        {String}        matches Hammer.EVENT_START|MOVE|END
srcEvent         {Object}        the source event, like TouchStart or MouseDown *
startEvent       {Object} 
*/
