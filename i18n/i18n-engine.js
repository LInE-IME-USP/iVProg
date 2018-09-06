
var LocalizedStrings = ivprogCore.LocalizedStrings;
var StringTypes = ivprogCore.StringTypes;

function i18n(identifier) {
  var opts = identifier.split(':');
  var type = opts[0].toLowerCase();
  var id = opts[1];
  if (StringTypes.ERROR === type) {
    return LocalizedStrings.getError(id);
  } else if (StringTypes.MESSAGE === type) {
    return LocalizedStrings.getMessage(id); 
  } else if (StringTypes.UI === type) {
    return LocalizedStrings.getUI(id);
  } else {
    console.warn(" A string has been passed to the i18n helper function that was not in the form type:id -> " + identifier);
    return LocalizedStrings.getString(id, type);
  }
}

i18n.set = function(locale, identifier, translate) {
  if (!i18n.db[locale]) {
    i18n.db[locale] = {};
  }
  i18n.db[locale][identifier] = translate;
}

i18n.updateLocale = function(new_locale) {
  localStorage.setItem('ivprog.lang', new_locale);
  $( "data.i18n" ).each(function() {
    $( this ).text(i18n($( this ).val()));
  });
}

$( document ).ready(function() {
  $( "data.i18n" ).each(function() {
    $( this ).text(i18n($( this ).val()));
  });
});