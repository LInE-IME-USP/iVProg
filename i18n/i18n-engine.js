
function i18n(identifier) {
  if (!i18n.db[i18n.locale]) {
    if (!i18n.db['en'][identifier]) {
      return "{MISSING_I18N_IDENTIFIER}";
    }
    return i18n.db['en'][identifier];
  }
  if (!i18n.db[i18n.locale][identifier]) {
    return "{MISSING_I18N_IDENTIFIER}";
  }
  return i18n.db[i18n.locale][identifier];
}

i18n.set = function(locale, identifier, translate) {
  if (!i18n.db[locale]) {
    i18n.db[locale] = {};
  }
  i18n.db[locale][identifier] = translate;
}

i18n.updateLocale = function(new_locale) {
  i18n.locale = new_locale;
  $( "data.i18n" ).each(function( index ) {
    $( this ).text(i18n($( this ).val()));
  });
}

i18n.locale = iLMparameters.lang;
i18n.db = {};

$.ajaxSetup({
    async: false
});

$.getJSON('i18n/i18n-database.json', function(data) {
    for (x in data) {
      l = data[x];
      i18n.set('en', x, l.en);
      i18n.set('es', x, l.es);
      i18n.set('pt', x, l.pt);
    }
});

$.ajaxSetup({
    async: true
});

$( document ).ready(function() {
  $( "data.i18n" ).each(function( index ) {
    $( this ).text(i18n($( this ).val()));
  });
});