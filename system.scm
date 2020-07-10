;;; system.scm -- Guix package definition
;; Copyright © 2020 David Dashyan <mail@davie.li>
;;
;; You should have received a copy of the GNU General Public License
;; along with bigchaindb-guix.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (guix utils))

(set! %load-path (cons (current-source-directory) %load-path))
(use-modules (bigchaindb-guix os node))
(load "guix.scm")

(define (swap-package-in-place! os-declaration name package)
  "Swaps package namened NAME with package PACKAGE in OS-DECLARTION."
  (pair-for-each
   (lambda (x)
     (if (equal? (package-name (car x)) package-to-replace-name)
         (set-car! x package)))
   (operating-system-packages os-declaration))
  os-declaration)

bigchaindb-node
