define(["exports", "fable-core", "./Helpers", "./fable_external/Fable.Helpers.Virtualdom-1524740637"], function (exports, _fableCore, _Helpers, _FableHelpers) {
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });
  exports.Actions = exports.Model = undefined;
  exports.update = update;
  exports.view = view;
  exports.start = start;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  var _createClass = function () {
    function defineProperties(target, props) {
      for (var i = 0; i < props.length; i++) {
        var descriptor = props[i];
        descriptor.enumerable = descriptor.enumerable || false;
        descriptor.configurable = true;
        if ("value" in descriptor) descriptor.writable = true;
        Object.defineProperty(target, descriptor.key, descriptor);
      }
    }

    return function (Constructor, protoProps, staticProps) {
      if (protoProps) defineProperties(Constructor.prototype, protoProps);
      if (staticProps) defineProperties(Constructor, staticProps);
      return Constructor;
    };
  }();

  var Model = exports.Model = function () {
    function Model(input) {
      _classCallCheck(this, Model);

      this.Input = input;
    }

    _createClass(Model, [{
      key: "Equals",
      value: function Equals(other) {
        return _fableCore.Util.equalsRecords(this, other);
      }
    }, {
      key: "CompareTo",
      value: function CompareTo(other) {
        return _fableCore.Util.compareRecords(this, other);
      }
    }], [{
      key: "initial",
      get: function get() {
        return (0, _Helpers.isNotNull)(window.storage) ? window.storage : function () {
          var model = new Model("");
          window.storage = model;
          return model;
        }();
      }
    }]);

    return Model;
  }();

  _fableCore.Util.setInterfaces(Model.prototype, ["FSharpRecord", "System.IEquatable", "System.IComparable"], "App.Main.Model");

  var Actions = exports.Actions = function () {
    function Actions(caseName, fields) {
      _classCallCheck(this, Actions);

      this.Case = caseName;
      this.Fields = fields;
    }

    _createClass(Actions, [{
      key: "Equals",
      value: function Equals(other) {
        return _fableCore.Util.equalsUnions(this, other);
      }
    }, {
      key: "CompareTo",
      value: function CompareTo(other) {
        return _fableCore.Util.compareUnions(this, other);
      }
    }]);

    return Actions;
  }();

  _fableCore.Util.setInterfaces(Actions.prototype, ["FSharpUnion", "System.IEquatable", "System.IComparable"], "App.Main.Actions");

  function update(model, action) {
    var patternInput = [new Model(action.Fields[0]), new _fableCore.List()];
    window.storage = patternInput[0];
    return [patternInput[0], patternInput[1]];
  }

  function view(model) {
    return function () {
      var tagName = "div";
      return function (children) {
        return new _FableHelpers.Html.Types.DomNode("Element", [[tagName, new _fableCore.List()], children]);
      };
    }()(_fableCore.List.ofArray([function () {
      var tagName = "label";
      return function (children) {
        return new _FableHelpers.Html.Types.DomNode("Element", [[tagName, new _fableCore.List()], children]);
      };
    }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.DomNode("Text", ["Enter name abc: "])])), new _FableHelpers.Html.Types.DomNode("VoidElement", [["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("EventHandler", [["oninput", function (e) {
      return function (arg0) {
        return new Actions("ChangeInput", [arg0]);
      }(e.target.value);
    }]])])]]), new _FableHelpers.Html.Types.DomNode("VoidElement", [["br", new _fableCore.List()]]), function () {
      var tagName = "span";
      return function (children) {
        return new _FableHelpers.Html.Types.DomNode("Element", [[tagName, new _fableCore.List()], children]);
      };
    }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.DomNode("Text", [_fableCore.String.fsFormat("Hello %s")(function (x) {
      return x;
    })(model.Input)])]))]));
  }

  function start(node, unitVar1) {
    return _FableHelpers.App.start((0, _FableHelpers.renderer)(), function (app) {
      return _FableHelpers.App.withStartNodeSelector(node, app);
    }(_FableHelpers.App.createApp(Model.initial, function (model) {
      return view(model);
    }, function (model) {
      return function (action) {
        return update(model, action);
      };
    })));
  }
});
//# sourceMappingURL=Main.js.map