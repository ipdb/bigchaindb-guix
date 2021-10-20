;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 David Dashyan <mail@davie.li>
(define-module (bigchaindb-packages mongodb)
  #:use-module (gnu packages)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages base)
  #:use-module (gnu packages valgrind)
  #:use-module (guix build-system go)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public go-gopkg.in-mgo.v2
  (package
    (name "go-gopkg.in-mgo.v2")
    (version "2016.08.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-mgo/mgo")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rwbi1z63w43b0z9srm8m7iz1fdwx7bq7n2mz862d6liiaqa59jd"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gopkg.in/mgo.v2"
       ;; TODO: The tests fail as MongoDB fails to start
       ;; Error parsing command line: unrecognised option '--chunkSize'
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps)
         (add-before 'check 'start-mongodb
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "src/gopkg.in/mgo.v2"
                 (invoke "make" "startdb")))
             #t))
         (add-after 'check 'stop'mongodb
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "src/gopkg.in/mgo.v2"
                 (invoke "make" "stopdb")))
             #t)))))
    (native-inputs
     `(("go-gopkg.in-check.v1" ,go-gopkg.in-check.v1)
       ("mongodb" ,mongodb)
       ("daemontools" ,daemontools)))
    (synopsis "@code{mgo} offers a rich MongoDB driver for Go.")
    (description
     "@code{mgo} (pronounced as mango) is a MongoDB driver for the Go language.
It implements a rich selection of features under a simple API following
standard Go idioms.")
    (home-page "https://labix.org/mgo")
    (license license:bsd-2)))

(define-public mongodb
  (package
    (name "mongodb")
    (version "3.4.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mongodb/mongo/archive/r"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0y1669sqj8wyf0y0njhxs4qhn1qzjhrs2h2qllya5samxrlrjhkg"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each (lambda (dir)
                              (delete-file-recursively
                                (string-append "src/third_party/" dir)))
                            '("pcre-8.42" "scons-2.5.0" "snappy-1.1.3"
                              "valgrind-3.11.0" "wiredtiger"
                              "yaml-cpp-0.6.2" "zlib-1.2.11"))
                  #t))))
    (build-system scons-build-system)
    (inputs
     `(("openssl" ,openssl-1.0)
       ("pcre" ,pcre)
       ("yaml-cpp" ,yaml-cpp)
       ("zlib" ,zlib)
       ("snappy" ,snappy)))
    (native-inputs
     `(("valgrind" ,valgrind)
       ("perl" ,perl)
       ("python" ,python-2)
       ("python2-pymongo" ,python2-pymongo)
       ("python2-pyyaml" ,python2-pyyaml)
       ("tzdata" ,tzdata-for-tests)))
    (arguments
     `(#:scons ,scons-python2
       #:phases
       (let ((common-options
              `(;; "--use-system-tcmalloc" TODO: Missing gperftools
                "--use-system-pcre"
                "--wiredtiger=off"
                ;; TODO
                ;; build/opt/mongo/db/fts/unicode/string.o failed: Error 1
                ;; --use-system-boost
                "--use-system-snappy"
                "--use-system-zlib"
                "--use-system-valgrind"
                ;; "--use-system-stemmer" TODO: Missing relevant package
                "--use-system-yaml"
                "--disable-warnings-as-errors"
                ,(format #f "--jobs=~a" (parallel-job-count))
                "--ssl")))
         (modify-phases %standard-phases
           (add-after 'unpack 'patch
             (lambda _
               ;; Remove use of GNU extensions in parse_number_test.cpp, to
               ;; allow compiling with GCC 7 or later
               ;; https://jira.mongodb.org/browse/SERVER-28063
               (substitute* "src/mongo/base/parse_number_test.cpp"
                 (("0xabcab\\.defdefP-10")
                  "687.16784283419838"))
               #t))
           (add-after 'unpack 'scons-propagate-environment
             (lambda _
               ;; Modify the SConstruct file to arrange for
               ;; environment variables to be propagated.
               (substitute* "SConstruct"
                 (("^env = Environment\\(")
                  "env = Environment(ENV=os.environ, "))
               #t))
           (add-after 'unpack 'create-version-file
             (lambda _
               (call-with-output-file "version.json"
                 (lambda (port)
                   (display ,(simple-format #f "{
    \"version\": \"~A\"
}" version) port)))
               #t))
           (replace 'build
             (lambda _
               (apply invoke `("scons"
                               ,@common-options
                               "mongod" "mongo" "mongos"))))
           (delete 'check) ;; XXX: add tests
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "mongod" bin)
                 (install-file "mongos" bin)
                 (install-file "mongo" bin))
               #t))))))
    (home-page "https://www.mongodb.org/")
    (synopsis "High performance and high availability document database")
    (description
     "Mongo is a high-performance, high availability, schema-free
document-oriented database.  A key goal of MongoDB is to bridge the gap
between key/value stores (which are fast and highly scalable) and traditional
RDBMS systems (which are deep in functionality).")
    (license (list license:agpl3
                   ;; Some parts are licensed under the Apache License
                   license:asl2.0))))

(define-public mongodb-bin
  (package
    (inherit mongodb)
    (source (origin
              (method url-fetch/tarbomb)
              (uri "https://fastdl.mongodb.org/linux/mongodb-linux-x86_64-3.4.24.tgz")
              (sha256
               (base32
                "1ik3hvljngsnzdpv2jjybja6xwcf4bplc8242xqpl3cx7g7115y4"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("mongodb-linux-x86_64-3.4.24/" "."))
       #:validate-runpath? #f))))

(define-public mongo-tools
  (package
    (name "mongo-tools")
    (version "3.4.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/mongodb/mongo-tools")
                   (commit (string-append "r" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1bcsz5cvj39a7nsxsfqmz9igrw33j6yli9kffigqyscs52amw7x1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mongodb/mongo-tools"
       #:modules ((srfi srfi-1)
                  (guix build go-build-system)
                  (guix build utils))
       #:install-source? #f
       #:phases
       (let ((all-tools
              '("bsondump" "mongodump" "mongoexport" "mongofiles"
                "mongoimport" "mongooplog" "mongorestore"
                "mongostat" "mongotop")))
         (modify-phases %standard-phases
           (add-after 'unpack 'delete-bundled-source-code
             (lambda _
               (delete-file-recursively
                "src/github.com/mongodb/mongo-tools/vendor")
               #t))
           (add-after 'delete-bundled-source-code 'patch-source
             (lambda _
               ;; Remove a redundant argument that causes compilation to fail.
               (substitute*
                   "src/github.com/mongodb/mongo-tools/mongorestore/filepath.go"
                 (("skipping restore of system.profile collection\", db)")
                  "skipping restore of system.profile collection\")"))
               #t))
           (replace 'build
             (lambda _
               (for-each (lambda (tool)
                           (let ((command
                                  `("go" "build"
                                    ;; This is where the tests expect to find the
                                    ;; executables
                                    "-o" ,(string-append
                                           "src/github.com/mongodb/mongo-tools/bin/"
                                           tool)
                                    "-v"
                                    "-tags=\"ssl sasl\""
                                    "-ldflags"
                                    "-extldflags=-Wl,-z,now,-z,relro"
                                    ,(string-append
                                      "src/github.com/mongodb/mongo-tools/"
                                      tool "/main/" tool ".go"))))
                             (simple-format #t "build: running ~A\n"
                                            (string-join command))
                             (apply invoke command)))
                         all-tools)
               #t))
           (replace 'check
             (lambda _
               (with-directory-excursion "src"
                 (for-each (lambda (tool)
                             (invoke
                              "go" "test" "-v"
                              (string-append "github.com/mongodb/mongo-tools/"
                                             tool)))
                           all-tools))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each (lambda (tool)
                           (install-file
                            (string-append "src/github.com/mongodb/mongo-tools/bin/"
                                           tool)
                            (string-append (assoc-ref outputs "out")
                                           "/bin")))
                         all-tools)
               #t))))))
    (native-inputs
     `(("go-github.com-howeyc-gopass" ,go-github.com-howeyc-gopass)
       ("go-github.com-jessevdk-go-flags" ,go-github.com-jessevdk-go-flags)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-gopkg.in-mgo.v2" ,go-gopkg.in-mgo.v2)
       ("go-gopkg.in-tomb.v2" ,go-gopkg.in-tomb.v2)
       ("go-github.com-nsf-termbox-go" ,go-github.com-nsf-termbox-go)
       ("go-github.com-smartystreets-goconvey" ,go-github.com-smartystreets-goconvey)))
    (home-page "https://github.com/mongodb/mongo-tools")
    (synopsis "Various tools for interacting with MongoDB and BSON")
    (description
     "This package includes a collection of tools related to MongoDB.
@table @code
@item bsondump
Display BSON files in a human-readable format
@item mongoimport
Convert data from JSON, TSV or CSV and insert them into a collection
@item mongoexport
Write an existing collection to CSV or JSON format
@item mongodump/mongorestore
Dump MongoDB backups to disk in the BSON format
@item mongorestore
Read MongoDB backups in the BSON format, and restore them to a live database
@item mongostat
Monitor live MongoDB servers, replica sets, or sharded clusters
@item mongofiles
Read, write, delete, or update files in GridFS
@item mongooplog
Replay oplog entries between MongoDB servers
@item mongotop
Monitor read/write activity on a mongo server
@end table")
    (license license:asl2.0)))
