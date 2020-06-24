console.log('kicking off the script');

HTMLWidgets.widget({
  name: 'rhot',
  type: 'output',
  factory: function(el, width, height) {

  console.log('beginning of the factory function')

    // #### RENDERING FUNCTION ####
    renFunc = function(x) {
      console.log(x);
      // use the parent element to control widget sizing
      // but don't pass these on to the constructor
      el.style.overflow = x.sizeInfo.overflow;
      el.style.height = x.sizeInfo.height;
      el.style.width = x.sizeInfo.width;


      el.params = x.params;

      el.hot = new Handsontable(el, x);

      el.hot.params = x.params;
      console.log(el.hot);
      el.hot.render();
    };

    // resizing function
    resFunc = function(width, height) {
      console.log('Entered resizing function.');
      console.log(this);
      //console.log(this.hot.getSettings('columns'));

      this.width = width;
      this.height = height;
      //this.hot.render();
    };

    // wwe want all these callbacks set upon initialization of the widget
    // so we bundle them into one function. each one will get called with hot
   /* setup_callbacks = function(hot){
      // these should only get set for a shiny widget
      console.log(HTMLWidgets.shinyMode);

      if( !HTMLWidgets.shinyMode ){ return }

      hot.afterChange = function(changes, source){
        // there are many ways this can be triggered
        // for now i only want to react if its an "edit" event
        console.log(changes);
        console.log(source);
        if ( source == 'edit' ){

        }
      };
    };
*/


    // return object full of all our code for the widget
    return {
      renderValue: renFunc,
      resize: resFunc
    };
  }
  // end factory

});



    /*  // there are several plugins; for cleanliness I get their references here
      var manualColumnResizePlugin = hot.getPlugin('manualColumnResize');
      var columnSortingPlugin = hot.getPlugin('columnSorting');
      if( x.manualColumnResize )
        manualColumnResizePlugin.enablePlugin();
      columnSortingPlugin.enablePlugin();
    */
