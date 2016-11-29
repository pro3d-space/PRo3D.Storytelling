define(["module", "exports", "./Helpers", "./Main"], function (module, exports, _Helpers, _Main) {
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });
  exports.node = exports.contentNode = undefined;
  var contentNode = exports.contentNode = "#app";
  var node = exports.node = document.querySelector(contentNode);

  if ((0, _Helpers.isNotNull)(module.hot)) {
    module.hot.accept();
    module.hot.dispose(function (_arg1) {
      node.removeChild(node.firstChild);
    });
  }

  (0, _Main.start)(contentNode);
});
//# sourceMappingURL=Entry.js.map