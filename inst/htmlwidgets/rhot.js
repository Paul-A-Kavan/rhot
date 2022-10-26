
HTMLWidgets.widget({
  name: 'rhot',
  type: 'output',
  factory: function(el, width, height) {
    renFunc = function(x) {
      // use the parent element to control widget sizing but don't pass these on to the constructor
      el.style.overflow = x.sizeInfo.overflow;
      el.style.width = x.sizeInfo.width;
      el.style.height = x.sizeInfo.height;

      el.params = x.params;
      //el.hot.params = x.params;

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
            new Function("return (" + currentElement.editor +")")()
          );
        }
      );

      //instantiate the hot object
      el.hot = new Handsontable(el, x);

      // Register hook listeners after the table is instantiated
      x.hooks.forEach(
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
