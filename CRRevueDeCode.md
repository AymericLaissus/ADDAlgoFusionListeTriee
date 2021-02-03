# Compte-Rendu de Revue de Code

  Par Clément Guilbert

## Introduction

La revue de code a commencé par un problème majeur : mes connaissances en programmation fonctionnelle et en Scheme sont un peu dépassées. Il a donc fallu réinstaller de quoi exécuter le code et s'en rappeler les principaux concepts pour pouvoir l'apréhender

## Tests
Dans un premier temps, j'ai testé le code a la main après une lecture de celui-ci et pu identifier un premier problème, une erreur lors de l'ajout d'un noeud vide. Une autre erreur provenait de la surcharge d'arguments, par exemple lors que l'on passait "1 8" en paramètre du menu principal.

Ensuite, j'ai recherché des moyens d'implémenter des tests unitaires en Scheme. Etant un langage relativement peu commun, il a été fastidieux de trouver des sources fiables, à jour et fonctionnelles.

Voici l'implémentation des tests unitaires que j'ai pu utiliser :

```scheme
 ,open handle ; we need `with-handler'

 (define *tests* '())
 (define *test-failures* '())

 ;;
 ;; 'assert' comes from Rolf-Thomas Hoppe's 'krims' package at 
 ;; [http://www.scsh.net/resources/sunterlib.html](http://www.scsh.net/resources/sunterlib.html)
 ;;

 (define-syntax assert
	(syntax-rules ()
	((assert ?x ?y0 ...)
		(if (not ?x) (error "Assertion failed" '?x ?y0 ...))) ))

 (define-syntax define-tests
   (syntax-rules ()
     ((define-tests ?suite-name ?bindings (?name1 ?body1 ...) ...)
      (set-test-suite! '?suite-name
                       (list (cons '?name1 (lambda() (let* ?bindings ?body1 ...))) ...)))))

 (define (set-test-suite! name tests)
	(if (assq name *tests*)
	(set-cdr! (assq name *tests*) tests)
	(set! *tests* (alist-cons name tests *tests*))))

 (define (find-test-by-name suite-name test-name default-thunk)
	(let* ((suite (assq suite-name *tests*))
		(test (assq test-name (cdr suite))))
	(if (not test)
		default-thunk
		(cdr test))))

 (define (setup-thunk suite-name)
  (find-test-by-name suite-name 'setup (lambda () #f)))

 (define (teardown-thunk suite-name)
	(find-test-by-name suite-name 'teardown (lambda () #f)))

 (define (test-thunks suite-name)
  ;; Returns the test routines, filtering out 'setup and 'teardown forms.
  (let ((suite (assq suite-name *tests*)))
	(remove (lambda (tst) (or (eq? 'setup (car tst))
				(eq? 'teardown (car tst))))
	  (cdr suite))))

 (define (with-handle-test-error* suite-name test-name thunk)
  (call-with-current-continuation
	(lambda (k)
	(with-handler
		(lambda (condition next)
		(set! *test-failures* 
			(cons (list suite-name test-name condition) *test-failures*))
		(k '()))
		thunk))))

 (define-syntax with-handle-test-error
	(syntax-rules ()
	((with-handle-test-error ?suite-name ?test-name ?body ...)
		(with-handle-test-error* ?suite-name ?test-name (lambda () ?body ...)))))

 (define (display-failures test-failures)
	(for-each (lambda (failure)
		  (display "FAILURE: ")
		  (display failure)
		  (newline)) test-failures))

 (define (run-tests)
	(set! *test-failures* '())
	(for-each (lambda (suite)
		  (run-test-suite (car suite)))
		  *tests*)
	(display-failures *test-failures*))

 (define (run-test-suite suite-name)
	(let ((suite (assq suite-name *tests*)))
	(if (not suite)
		(error "Suite " suite-name "not defined"))
	(for-each (lambda (tst)
			(with-handle-test-error suite-name (car tst)
					 (run-test suite-name (car tst))))

		(test-thunks suite-name))))

 (define (run-test suite-name test-name)
	(dynamic-wind (setup-thunk suite-name)
	(find-test-by-name suite-name test-name 'test-not-found) 
	(teardown-thunk suite-name)))

```

Lien vers la documentation complète : https://wiki.c2.com/?SchemeUnit

### Tests réalisés
```scheme
(define ABR ())

(abr-ajout ABR 1)
(abr-ajout ABR 3)
(abr-ajout ABR 5)

(define-tests abr-tests
	((ABR ()))
        (setup (display "SETUP"))
        (teardown (display "TEARDOWN"))
        (test-ajout
        (assert (= (display ABR) (1 () (3 () (5 () ())))))
        (assert (= (abr-membre? ABR 3) #t))))

        
(abr-supp ABR 3)

(define-tests abr-tests2
	((ABR ()))
        (setup (display "SETUP"))
        (teardown (display "TEARDOWN"))
        (test-suppr-parcours
        (assert (= (display ABR) (1 () (5 () ()))))
        (assert (= (abr-membre? ABR 3) #t))
        (assert (= (abr-parcours-ordonne ABR) (1 5)))))
        
(abr-creer-vide)

(define-tests abr-tests3
	((ABR ()))
        (setup (display "SETUP"))
        (teardown (display "TEARDOWN"))
        (test-taille-arbre-vide
        (assert (= (abr-taille ABR) 0))))

(define-tests abr-err
        ((ABR ()))
        (setup (display "SETUP"))
        (teardown (display "TEARDOWN"))                                                                                                                                                                        (test-err
        (assert (= (abr-ajout ABR #f) () ))))

```
Le test "abr-err" nous aura permis de trouver un cas particulier. En effet, il était censé mettre en valeur l'erreur trouvée à la main, lors du passage d'une valeur incorrecte à la fonction d'ajout. Le test est cependant passé : en vérifiant à la main, le passage de l'argument #f à la procédure d'ajout passe (contrairement à d'autres valeurs non entières). On peut ainsi avoir un noeud #f dans un arbre, entrainant un comportement imprévu pour la suite de l'exécution.

## Remarques
Dans la forme, j'ai pu remarquer que la gestion des choix était affichée en anglais alors que le reste du programme était en français. Une uniformisation permet d'avoir une expérience utilisateur plus cohérente.

## Erreurs relevées
```scheme
Enter choice [1-8] >1
Entrer la valeur à ajouter >

;The object [...] is not the correct type.
```
```scheme
Enter choice [1-8] >1 8

;The object [...] is not the correct type.
```

Ajout possible de #f à un arbre lors de l'initialisation.