-module(inet_test).
-compile(export_all).

% inet:close(Socket) cierra un socket
% inet:get_rc() -> Retorna la lista de configuration de los sockets
% inet:format_error(Error) -> Formatea el error, por ejemplo, inet:format_error(eacces)
% inet:getaddr(Host, inet|inet6) -> Obtiene el ip de un host
% inet:getaddrs(Host, inet|inet6) -> Obtiene todas las ips de un host
% inet:gethostbyaddr(Address) -> Obtiene el hostname de una ip
% inet:gethostname()
% inet:getifaddrs()


% Opciones:
% {active, true|false|once|N} Si el valor es true (por defecto), todo lo que recibe el socket se envia al proceso como mensaje. Si el valor es
% falso, el proceso debe llamar a get_tcp:recv o gen_udp:recv o gen_sctp:recv para recibir datos. Si el valor es once, un mensaje es enviado
% del socket al proceso, y para recibir otro tiene que llamar a setopts con {active, once}, si el valor es N, el socket manda N mensajes
% como reciba datos y cuando es 0 deja de mandar y hay que llamar de nuevo a setopts(Socket, {active, N}) para recibir mas mensajes.
% {broadcast, true|false} da o quita permisos para hacer broadcast, solo para sockets udp
% {delay_send, true|false} cuando se tiene datos para enviar, erlang trata de mandar todo enseguida, y si falla, se pone la informacion en cola
% y cuando el OS avisa que esta listo para mandar datos, se manda lo que esta en la cola. Si esta opcion esta en true, se pone los datos en cola
% y se van enviando los datos en menos paquetes y mas largos
% {deliver, port|term} cuando active es true, puede mandar los mensajes de la forma port {Socket, {data, [H1|Data]}} o term {tcp, Socket, [H1|Data]}
% {dontroute, true|false} permite o desactiva el ruteo para los mensajes
% {exit_on_close, true|false} por defecto es true, mantiene el socket abierto incluso cuando se detecto que el otro par finalizo
% {header, Size} Sirve para sockets binarios, si Size es 3, va a recibir {Byte1, Byte2, Byte3 | Binary}, es decir, los primeros Size elementos
% son convertidos a bytes y el resto es un bitstring
% {high_msgq_watermark, Size} Por defecto 8kB, la cola de mensajes del socket va a ser puesta en estado "ocupado" cuando esta alcanze el limite Size.
% {high_watermark, Size} Para sockets TCP/IP, el socket va a ser puesto en estado "ocupado" cuando los datos para el socket alcancen este limite, que
% por defecto es 8 kB.
% {ipv6_v6only, true|false} Restringe el socket a conexiones IPv6, restringiendo conexiones IPv4
% {keepalive, true|false} por defecto false, envia informacion periodicamente para evitar que se cierre el socket, if el otro par no response se
% cierra la conexion
% {linger, true|false, Seconds} determina el timeout en Seconds para esperar que se envie los datos en la cola cuando se llama a close para cerrar
% el socket
% {low_msgq_watermark, Size} Si la cola de mensajes de socket esta en estado "ocupado", va a salir del estado cuando se alcance este limite, que
% por defecto es 4kB
% {low_watermark, Size} Para sockets TCP/IP, si el socket esta en estao "ocupado", va a salir del estado cuando los datos a trasmitir alcancen
% este limite, que por defecto es 4kB
% {mode, binary|list} Los paquetes que se reciben, se reciben como binarios o listas.
% list Es como usar {mode, list}
% binary Es como usar {mode, Binary}
% {nodelay, true|false} cuando es true, setea la opcion TCP_NODELAY, donde se envia/recibe informacion ni bien este lista, desactivango el algoritmo
% de Nagle
% {packet, TipoPaquete} Define el tipo de paquete a user para el socket, pudiendo ser:
%          raw | 0 -> No se hace empaquetado
%          1 | 2 | 4 -> El paquete tiene una cabecera que especifica la cantidad de bytes del paquete seguida por el numero de bytes especificados
%                       como numero
%          asn1 | cdr | sunrm | fcgi | tpkt | line -> influye en como se recibe el mensaje, pero no como se envia, significados:
%                       asn1 -> ASN.1 BER
%                       sunrm -> Sun's RPC encoding
%                       cdr -> CORBA (GIOP 1.1)
%                       fcgi -> FastCGI
%                       tpkt -> Format TPKT
%                       line -> Un paquete termina con una nueva linea, lineas mas largas que el paquete son truncadas.
% http | http_bin -> El protocolo HTTP utilizado, 
% httph | httph_bin -> Por lo general no se necesita, los cambios de http|http_bin a httph|httph_bin suceden luego de leer la primera linea
% {packet_size, Entero} -> Setea el maximo permitido para el cuerpo del paquete.
% {priority, Prioridad} -> setea la prioridad definida por el protocolo para todos los paquetes enviados con el socket
% {raw, Protocolo, NumeroOpcion, ValorDeOpcion} raw sockets, casi no se usa, son las opciones de setsockopt en c
% {read_packets, Entero} -> Para sockets UDP, especifica la cantidad de paquetes a leer sin la intervencion del socket cuando hay datos disponibles.
%                            Por defecto es 5.
% {recbuf, Tamanio} -> El tamanio minimo para el buffer que recibe datos. El nro por defecto viene dado por el sistema operativo
% {reuseaddr, true|false} -> Permite o no la reutilizacion de puertos locales. Por defecto es false.
% {send_timeout, Entero} -> Para sockets TCP, la cantidad de tiempo a esperar para la confirmacion que los datos fueron enviados.
% {send_timeout_close, true|false} -> Para sockets TCP, especifica si se cierra el socket en un timeout, por defecto es false, se recomienda true.
% {sndbuf, Tamanio} -> El tamanio minio para el buffer que envia datos. El nro por defecto viene dado por el sistema operativo.
% {priority, Entero} -> Setea la prioridad del socket.
% {tos, Entero} -> setea el TOS (Tipo de Servicio), se utiliza para priorizar paquetes en la red.

% inet:setopts(Socket, Options)
% inet:getopts(Socket, Options)
