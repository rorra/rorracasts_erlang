{application, info_app,
 [
  {description, "Mi primer app"},
  {vsn, "1"},
  {modules, [dynamic, info, super]},
  {registered, [super]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {info_app,[]}}
 ]
}.
