var Point = {
    new: function(x, y) {
        return { X: x, Y: y };
    },

    scale: function(p, s) {
        return this.new(p.X * s, p.Y * s);
    },

    add: function(a, b) {
        return this.new(a.X + b.X, a.Y + b.Y);
    },

    sub: function(a, b) {
        return this.new(a.X - b.X, a.Y - b.Y);
    },

    mul: function(a, b) {
        return this.new(a.X * b.X, a.Y * b.Y);
    },

    div: function(a, b) {
        return this.new(a.X / b.X, a.Y / b.Y);
    },

    length: function(p) {
        return Math.sqrt(p.X * p.X + p.Y * p.Y);
    },

    normalize: function(p) {
        var l = this.length(p);
        return this.new(p.X / l, p.Y / l);
    }
}

var baseSize = Point.new(1, 1);
var viewport = Point.new(1, 1);
var drag = { elem: null, prev: null };
var resize = { elem: null, prev: null };

// Sets the viewport size
function setViewport(size) {
    viewport = size;
    transformAll();
}

// Sets the base size
function setBaseSize(size) {
    baseSize = size;
    transformAll();
}

// Transforms all labels and arrows
function transformAll() {
    $('.annotations .annotation').find('.label').each(function () {
        transform($(this));
    });

    $('.annotations .annotation').find('.arrow').each(function () {
        setArrowOrigin($(this));
    });
}

// Computes the scale of an element based on the current size
// of the viewport and the base size
function getScale() {
    return Point.div(viewport, baseSize);
}

// Gets the corresponding arrow of a label
function getArrow(label) {
    return label.parents('.annotation')
                .find('.arrow');
}

// Gets the corresponding label of an arrow
function getLabel(arrow) {
    return arrow.parents('.annotation')
                .find('.label');    
}

// Transforms the given label based on its scale and position
function transform(elem) {
    var pos = elem.data('pos');
    var scale = getScale();
    var globalScale = Math.min(scale.X, scale.Y);

    elem.css({
        'transform': `translate(${pos.X * scale.X}px, ${pos.Y * scale.Y}px) scale(${globalScale})`
    });    
}

// Adjusts the height of the given label based on its
// textarea
function autosize(elem) {
    elem.children('textarea')
		.height('auto')
        .height(function() {
            return `${this.scrollHeight}px`;
        });

    var arrow = getArrow(elem);
    if (arrow.length > 0) {
        setArrowOrigin(arrow);
    }     
}

// Sets the text of the given label textarea
function setText(elem, text) {
    elem.empty()
        .append(text);
        
    autosize(elem.parent());
}

// Sets the font size for the given label textarea
function setFontSize(elem, size) {
    elem.css('font-size', `${size}px`);
    autosize(elem.parent());
}

// Sets the width of the given label
function setWidth(elem, width) {
    elem.width(width);
    autosize(elem);
}

// Sets the position of the given label
function setPosition(elem, pos) {
    elem.data('pos', pos);
    transform(elem);

    var arrow = getArrow(elem);
    if (arrow.length > 0) {
        setArrowOrigin(arrow);
    }    
}

// Makes the given label the top label
function setTop(elem) {
    $('.annotations .top.label').each(function () {
        $(this).removeClass('top');
    });
    elem.addClass('top');
}

// Arrows
function setArrowOrigin(arrow) {
    var label = getLabel(arrow);
    if (label.length === 0) {
        return;
    }

    var scale = getScale();
    var globalScale = Math.min(scale.X, scale.Y);

    var p = label.data('pos');
    var w = label.width() * globalScale / scale.X;
    var h = label.height() * globalScale / scale.Y;

    var target = arrow.data('target');
    var center = Point.new(p.X + w / 2, p.Y + h / 2);

    var anchors = [];
    anchors.push( Point.new(center.X - w / 2, center.Y) );
    anchors.push( Point.new(center.X + w / 2, center.Y) );
    anchors.push( Point.new(center.X, center.Y - h / 2) );
    anchors.push( Point.new(center.X, center.Y + h / 2) );

    var origin = anchors.map(function (a) {
        var dc = Point.sub(a, center);
        var dt = Point.sub(a, target);

        if (dc.X != 0) {
            dc = dc.X; dt = dt.X;
        } else {
            dc = dc.Y; dt = dt.Y;
        }

        if (Math.sign(dc) != Math.sign(dt)) {
            return { a:a, d:Math.abs(dt) };
        } else {
            return { a:a, d:-1 };
        }
    }).reduce(function (max, x) {
        return (x.d > max.d) ? x : max;
    }).a

    var midpoint = Point.new(origin.X + 10 * Math.sign(origin.X - center.X),
                             origin.Y + 10 * Math.sign(origin.Y - center.Y));

    arrow.data('origin', origin)
         .data('midpoint', midpoint);
    transformArrow(arrow);
}

function setArrowTarget(arrow, t) {
    arrow.data('target', t);
    setArrowOrigin(arrow);

    transformArrow(arrow);
}

function setArrowHeadSize(arrow, s) {
    var marker = arrow.children('defs')
                      .children('.arrowHead');

    marker.attr('viewBox', `0 0 ${s} ${s}`)
          .attr('markerWidth', s)
          .attr('markerHeight', s)
          .attr('refX', 0)
          .attr('refY', s / 2)
          .children('path')
          .attr('d', `M 0 0 L ${s} ${s / 2} L 0 ${s} z`);

    arrow.data('marker-size', s);

    arrow.children('.arrowLine')
         .attr('marker-end', `url(#${marker.attr('id')})`);
}

function transformArrow(arrow) {
    // Apply scale to the points
    var scale = getScale();
    var lineWidth = 3 * Math.min(scale.X, scale.Y, 1.0);
    
    var pts = [];
    pts.push(arrow.data('origin'));
    pts.push(arrow.data('midpoint'));
    pts.push(arrow.data('target'));

    pts.forEach(function (a, i) {
        pts[i] = Point.mul(a, scale);
    });

    // Compute the direction and length of the two segments
    // and the direct one
    var dir = [];
    var len = [];

    dir[0] = Point.sub(pts[1], pts[0]);
    dir[1] = Point.sub(pts[2], pts[1]);
    dir[2] = Point.add(dir[0], dir[1]);

    dir.forEach(function (a, i) {
        len[i] = Point.length(a);
        dir[i] = Point.scale(a, 1 / len[i]);
    });

    // Get the size of the arrow head (scaled by the line width since
    // "markerWidth" is "strokeWidth" by default)
    var headSize = arrow.data('marker-size') * lineWidth;

    // Compute the actual points of the polyline. We want
    // the end point to be shifted by the marker size so that the
    // end of the marker points exactly on the given end point
    var points = null;

    if (len[1] > headSize && len[0] < len[2]) {
        var endPoint = Point.add(pts[1], Point.scale(dir[1], len[1] - headSize));
        points = `${pts[0].X},${pts[0].Y} ${pts[1].X},${pts[1].Y} ${endPoint.X},${endPoint.Y}`;
    } else if (len[2] > headSize) {
        // Second segment is too short, or direct line would be shorter than the first segment
        // Just draw a straigh arrow in these cases
        var endPoint = Point.add(pts[0], Point.scale(dir[2], len[2] - headSize));
        points = `${pts[0].X},${pts[0].Y} ${endPoint.X},${endPoint.Y}`;
    }

    arrow.children('.arrowLine')
        .css('stroke-width', `${lineWidth}`)
        .attr('points', points);
}

// Initializes the label for the Javascript
// side (dragging, resizing, ...)
function initLabel (elem) {
    if (elem.hasClass('disabled')) {
        return;
    }

    elem.on('mouseenter', function() { if (drag.elem === null) elem.addClass('hovered'); })
        .on('mouseleave', function() { elem.removeClass('hovered'); });

    elem.children('textarea')
		.on('focus', function() { elem.addClass('focused'); })
		.on('blur', function() { elem.removeClass('focused'); })
		.on('input', function() { autosize(elem); });

    elem.children('.move.button').on('mousedown', function(ev) {
        setTop(elem);
		drag.elem = elem;
		drag.prev = Point.new(ev.clientX, ev.clientY);
    });
	
	elem.children('.grabber').on('mousedown', function(ev) {
		resize.elem = elem;
		resize.prev = ev.clientX;
    });
}

// Resizing
function resizeLabel(ev) {
    if (resize.elem == null) {
        return;
    }
    var scale = getScale();

    var curr = ev.clientX;
    var delta = (curr - resize.prev) / scale.X;
    resize.prev = curr;

    var w = resize.elem.width();
    setWidth(resize.elem, Math.max(100, Math.round(w + delta)));
}

function resizeEnd() {
    var elem = resize.elem;

	if (elem !== null) {
        aardvark.processEvent(elem.attr('id'), 'onlabelresized', elem.width());
    }
    
    resize.elem = null;
}

// Dragging
function dragLabel(ev) {
    var elem = drag.elem;

    if (elem == null) {
        return;
    }

    var scale = getScale();

    var curr = Point.new(ev.clientX, ev.clientY);
    var delta = Point.div(Point.sub(curr, drag.prev), scale);
    drag.prev = curr;

    // Drag the label
    var pos = elem.data('pos');
    setPosition(elem, Point.new(Math.round(pos.X + delta.X),
                                Math.round(pos.Y + delta.Y)));
}

function dragEnd() {
    var elem = drag.elem;

	if (elem !== null) {
        aardvark.processEvent(elem.attr('id'), 'onlabelmoved', elem.data('pos'));
    }
    
    drag.elem = null;
}

// Register event listeners
document.addEventListener('mousemove', function(ev) {
	dragLabel(ev);
	resizeLabel(ev);
});

document.addEventListener('mouseup', function() {
	dragEnd();
	resizeEnd();
});

window.addEventListener('resize', function() {
    $('.annotations .label').each(function() {
        transform($(this)); 
    });
});