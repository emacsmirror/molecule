
# &Iacute;ndice

1.  [molecule.el](#orgc22b3fa)
    1.  [English](#org1d37bcb)
        1.  [About](#org19dab99)
        2.  [Requirements](#org4e6fef8)
        3.  [Install](#org5ca9a55)
        4.  [Configuration](#org4765744)
        5.  [Advanced usage](#orga3aecca)
        6.  [License](#org185e017)
        7.  [Bugs, patches and feature requests](#org1031587)
    2.  [Castellano](#orgefd634c)
        1.  [Acerca de](#org65dc9a7)
        2.  [Requisitos](#org0609d13)
        3.  [Instalar](#orgeba4a95)
        4.  [Configuración](#org1e93569)
        5.  [Uso avanzado](#org894510a)
        6.  [Licencia](#org3e24497)
        7.  [Bugs, parches y solicitudes de características](#orgc020732)



<a id="orgc22b3fa"></a>

# molecule.el


<a id="org1d37bcb"></a>

## English


<a id="org19dab99"></a>

### About

Molecule is a testing framework for ansible roles. If you don't know what it is, you should probably learn about it on [their site](https://readthedocs.org/projects/molecule/) and then come back here. Molecule.el is a simple wrapper around molecule. Right now, it allows you to create roles and scenarios and execute all commands except `login`.


<a id="org4e6fef8"></a>

### Requirements

    sudo pip install molecule docker-py


<a id="org5ca9a55"></a>

### Install

You can install from Melpa for a stable release or from the git repository, which may be experimental.

    git clone https://git.daemons.it/drymer/molecule.el ~/.emacs.d/lisp/molecule.el

[![img](http://melpa.org/packages/molecule-badge.svg)](http://melpa.org/#/molecule)


<a id="org4765744"></a>

### Configuration

A minimal configuration would be this.

    (add-to-list 'load-path "~/.emacs.d/lisp/molecule.el/")
    (require 'molecule)
    (add-hook 'yaml-mode-hook #'molecule-mode)

For a more advanced configuration check the next chapter.


<a id="orga3aecca"></a>

### Advanced usage

1.  Main commands:

    -   `molecule-init`: Initialize a new role or scenario. Binded to `C-x m n`.

    -   `molecule-check`: Use the provisioner to perform a Dry-Run. Binded to `C-x m c`.

    -   `molecule-converge`: Use the provisioner to configure instances. Binded to `C-x m o`.

    -   `molecule-create`: Start instances. Binded to `C-x m r`.

    -   `molecule-dependency`: Mange the role's dependencies. Binded to `C-x m d`.

    -   `molecule-destroy`: Destroy instances. Binded to `C-x m e`.

    -   `molecule-idempotence`:  Do an idempotency test. Binded to `C-x m i`.

    -   `molecule-lint`: Lint the role. Binded to `C-x m l`.

    -   `molecule-list`: Lists status of instances. Binded to `C-x m s`.

    -   `molecule-syntax`: Use the provisioner to syntax check the role. Binded to `C-x m y`.

    -   `molecule-side-effect`: Use the provisioner to perform side-effects tests. Binded to `C-x m f`.

    -   `molecule-test`: Test (destroy, create, converge, lint, &#x2026;) Binded to `C-x m t`.

    -   `molecule-verify`: Run automated tests against instances. Binded to `C-x m v`.

    -   `molecule-debug`: Toggle debug on or off. Binded to `C-x m b`.

2.  Variables:

    -   `molecule-command`: It uses molecule in the path by default.

    -   `molecule-debug-v`: If set to `t`, it will pass the `--debug` flag to molecule. Set to nil by default.

3.  compile-mode

    Molecule uses `compile-mode` to execute molecule. By default it's a bit shitty, so you may add auto-scroll, colors and bind the `recompile` function to `C-,`:

        ;; Source: https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
        (use-package compile
          :config
          (defun colorize-compilation-buffer ()
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))
          (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
          (setq compilation-scroll-output t)
          :bind (
        	 ("C-," . recompile)))


<a id="org185e017"></a>

### License

    Author:: drymer <drymer [ AT ] autistici.org>
    Copyright:: Copyright (c) 2017, drymer

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or (at
    your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


<a id="org1031587"></a>

### Bugs, patches and feature requests

If you find a bug, have a patch or have a feature request, you may send an e-mail to the address in the previous section or go to [https://git.daemons.it/drymer/molecule.el/](https://git.daemons.it/drymer/molecule.el/)


<a id="orgefd634c"></a>

## Castellano


<a id="org65dc9a7"></a>

### Acerca de

Molecule es un framework de testeo de roles de ansible. Si no sabes que es, probablemente debas aprender sobre ello en [su página](https://readthedocs.org/projects/molecule/) y luego volver. Molecule.el es un wrapper simple alrededor de molecule. Ahora mismo, permite crear roles y escenarios y ejecutar todas las órdenes excepto `login`.


<a id="org0609d13"></a>

### Requisitos

    sudo pip install molecule docker-py


<a id="orgeba4a95"></a>

### Instalar

Se puede instalar desde Melpa para tener una versión estable o desde el repositorio git, que puede ser inestable.

    git clone https://git.daemons.it/drymer/molecule.el ~/.emacs.d/lisp/molecule.el/

[![img](http://melpa.org/packages/molecule-badge.svg)](http://melpa.org/#/nikola)


<a id="org1e93569"></a>

### Configuración

Una configuración minimalista sería la siguiente.

    (add-to-list 'load-path "~/.emacs.d/lisp/molecule.el/")
    (require 'molecule)
    (add-hook 'yaml-mode-hook #'molecule-mode)

Para una configuración más avanzada se puede consultar la sección siguiente.


<a id="org894510a"></a>

### Uso avanzado

1.  Comandos principales:

    -   `molecule-init`: Inicializa un nuevo rol o escenario. Asignado a `C-x m n`.

    -   `molecule-check`: Usa el provisionador para realizar una ejecución simulada&#x2026; Asignado a `C-x m c`.

    -   `molecule-converge`: Usr el provisionador para configurar instancias&#x2026; Asignado a `C-x m o`.

    -   `molecule-create`: Empezar instancias. Asignado a `C-x m r`.

    -   `molecule-dependency`: Manejar las dependencias del rol. Asignado a `C-x m d`.

    -   `molecule-destroy`: Destruir instancias. Asignado a `C-x m e`.

    -   `molecule-idempotence`: Usa el provisionador para configurarlo&#x2026; Asignado a `C-x m i`.

    -   `molecule-lint`: Pasarle el lint al role. Asignado a `C-x m l`.

    -   `molecule-list`: Lista los estados de las instancias. Asignado a `C-x m s`.

    -   `molecule-syntax`: Usa el provisionador para hacer un checkeo de sintaxis del role. Asignado a `C-x m y`.

    -   `molecule-side-effect`: Usa el provisionado para ejecutar posibles efectos secundarios. Asignado a `C-x m f`.

    -   `molecule-test`: Test (destroy, create, converge, lint,&#x2026; Asignado a `C-x m t`.

    -   `molecule-verify`: Ejecuta tests automatizados contra algunas instancias. Asignado a `C-x m v`.

    -   `molecule-debug`: Activar y desactivar el debug. Asignado a `C-x m b`.

2.  Variables:

    -   `molecule-command`: Usa molecule en el path por defecto.

    -   `molecule-debug-v`: Si se establece en `t`, se pasará el parámetro `--debug` a molecule. Establecido en nil por defecto.

3.  compile-mode

    Molecule usa `compile-mode` para ejecutar molecule. Por defecto es bastante mierdoso, así que igual es interesante añadirle auto-scroll, soporte de colores y asignar la función de `recompile` al atajo de teclado `C-,`:

        ;; Source: https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
        (use-package compile
          :config
          (defun colorize-compilation-buffer ()
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))
          (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
          (setq compilation-scroll-output t)
          :bind (
        	 ("C-," . recompile)))


<a id="org3e24497"></a>

### Licencia

    Autor:: drymer <drymer [EN] autistici.org>
    Derechos de autor:: Copyright (c) 2017, drymer

    Este programa es software libre: puedes redistribuirlo y/o modificarlo
    bajo los términos de la Licencia Pública General GNU publicada por
    la Free Software Foundation, ya sea la versión 2 de la Licencia, o
    su opción) cualquier versión posterior.
    Este programa se distribuye con la esperanza de que sea útil, pero
    SIN NINGUNA GARANTÍA; Sin la garantía implícita de
    COMERCIABILIDAD o APTITUD PARA UN PROPÓSITO PARTICULAR. Vea el GNU
    Licencia Pública General para más detalles.

    Debería haber recibido una copia de la GNU General Public License
    junto con este programa. Si no es así, consulte <http://www.gnu.org/licenses/>.


<a id="orgc020732"></a>

### Bugs, parches y solicitudes de características

Si encuentras un error, tienes un parche o tienes la solicitud de una característica, puedes enviar un correo electrónico a la dirección de la sección anterior o ir a [https://git.daemons.it/drymer/molecule.el](https://git.daemons.it/drymer/nikola.el).
