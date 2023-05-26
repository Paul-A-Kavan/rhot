
HTMLWidgets.widget({
  name: 'rhot',
  type: 'output',
  factory: function(el, width, height) {
    renFunc = function(x) {
      // use the parent element to control widget sizing but don't pass these on to the constructor
      el.style.overflow = x.sizeInfo.overflow;
      el.style.width = x.sizeInfo.width;
      el.style.height = x.sizeInfo.height;

      el.rParams = x.rParams;

      // Register the cell functions with Handsontable core before instantiating the hot object
      // Register renderers
      x.renderers.forEach(
        function(currentElement){
          Handsontable.renderers.registerRenderer(
            currentElement.name,
            new Function("return (" + currentElement.renderer +")")()
          );
        }
      );
      // Register validators
      x.validators.forEach(
        function(currentElement){
          Handsontable.validators.registerValidator(
            currentElement.name,
            new Function("return (" + currentElement.validator +")")()
          );
        }
      );
      // Register editors
      x.editors.forEach(
        function(currentElement){
          Handsontable.editors.registerEditor(
            currentElement.name,
            // by using a nested function we can make the r user's experience for editors more similar to the validators and renderers
            new Function("return (" + currentElement.editor + "(Handsontable) )")(Handsontable)
          );
        }
      );
     // Register global hook listeners before the table is instantiated
      x.hooksGlobal.forEach(
        function(currentElement) {
          Handsontable.hooks.add(
            currentElement.key,
            new Function("return (" + currentElement.callback +")")()
          );
        }
      );

      //instantiate the hot object
      el.hot = new Handsontable(el, x);

      // Register local hook listeners after the table is instantiated
      x.hooksLocal.forEach(
        function(currentElement) {
          el.hot.addHook(
            currentElement.key,
            new Function("return (" + currentElement.callback +")")()
          );
        }
      );

      el.hot.render();
    };

    resFunc = function(width, height) {};

    return {
      renderValue: renFunc,
      resize: resFunc
    };
  }
});
