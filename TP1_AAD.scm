; Créé un arbre binaire de recherche ABR vide
(define abr-creer-vide
  (lambda ()
    '()))

;Construit un ABR à partir d'une valeur de noeud, et de 2 ABR fils
(define abr-creer
  (lambda (valeur fils-gauche fils-droit)
    (list valeur fils-gauche fils-droit)))

;Teste si un ABR est vide ou non
(define abr-vide? (lambda (ABR) (null? ABR)))

;Récupère la valeur racine d'un ABR
(define abr-valeur (lambda (ABR) (car ABR)))

;Retourne le fils gauche d'un noeud d'ABR
(define abr-fils-gauche (lambda (ABR) (cadr ABR)))

;Retourne le fils droit d'un noeud d'ABR
(define abr-fils-droit (lambda (ABR) (caddr ABR)))

;Ajoute un nouveau noeud dans un ABR
(define abr-ajout
  (lambda (ABR valeur)
    (cond
      ((abr-vide? ABR)           ; si l'abr est vide on créé un nouveau noeud
       (abr-creer valeur (abr-creer-vide) (abr-creer-vide))) 
      ((< valeur (abr-valeur ABR))    ; ajout d'un noeud au fils gauche si la valeur ajoutée est inférieure à la valeur du noeud actuel
       (abr-creer (abr-valeur ABR)
                   (abr-ajout (abr-fils-gauche ABR) valeur)
                   (abr-fils-droit ABR)))
      ((> valeur (abr-valeur ABR))    ; ajout d'un noeud au fils droit si la valeur ajoutée est supérieure à la valeur du noeud actuel
       (abr-creer (abr-valeur ABR)
                   (abr-fils-gauche ABR)
                   (abr-ajout (abr-fils-droit ABR) valeur)))
      (else ABR))))

;Teste si l'ABR est une feuille (n'a pas de fils)
(define abr-estfeuille?
  (lambda (ABR)
    (and (abr-vide? (abr-fils-gauche ABR))
         (abr-vide? (abr-fils-droit ABR)))))

;Recherche le noeud le plus à gauche de l'ABR
(define abr-fils-plusagauche
  (lambda (ABR)
    (cond
      ((abr-vide? ABR) (abr-creer-vide))
      ((abr-vide? (abr-fils-gauche ABR)) ABR)
      (else (abr-fils-plusagauche (abr-fils-gauche ABR))))))

;Supprime la racine de l'ABR donné
(define abr-supp-noeud
  (lambda (ABR)
    (cond
      ; Si la racine n'a aucun fils on retourne un ABR vide
      ((abr-estfeuille? ABR) (abr-creer-vide))
      ; Si la racine a un seul enfant, celui-ci le remplace
      ((abr-vide? (abr-fils-gauche ABR)) (abr-fils-droit ABR))
      ((abr-vide? (abr-fils-droit ABR)) (abr-fils-gauche ABR))
      ; Si la racine a 2 enfants, il est remplacé par l'enfant le plus à gauche de l'enfant de droite
      (else (let* ((replacement-value
                     (abr-valeur (abr-fils-plusagauche (abr-fils-droit ABR))))
                   (new-right-subtree
                     (abr-supp (abr-fils-droit ABR) replacement-value)))
              (abr-creer replacement-value 
                          (abr-fils-gauche ABR) 
                          new-right-subtree))))))

;Supprime un noeud s'il existe
(define abr-supp
  (lambda (ABR valeur)
    (cond
      ((abr-vide? ABR) (abr-creer-vide)) ;Si l'ABR est vide on retourne un arbre vide
      ((= valeur (abr-valeur ABR)) (abr-supp-noeud ABR)) ;Si on a trouvé la valeur à supprimer on lui applique abr-supp-noeud
      ((< valeur (abr-valeur ABR)) ;Si la valeur à supprimer est inférieure à la valeur actuelle, on reconstruit un arbre en remplaçant le fils de gauche par une copie de celui-ci sans l'élément à supprimer
       (abr-creer (abr-valeur ABR)
                   (abr-supp (abr-fils-gauche ABR) valeur)
                   (abr-fils-droit ABR)))
      (else ;Si la valeur à supprimer est supérieure à la valeur actuelle, on reconstruit un arbre en remplaçant le fils de droit par une copie de celui-ci sans l'élément à supprimer
        (abr-creer (abr-valeur ABR)
                    (abr-fils-gauche ABR)
                    (abr-supp (abr-fils-droit ABR) valeur))))))

;Vérifie si une valeur est présente dans un ABR
(define abr-membre? 
  (lambda (ABR valeur)
    (cond
      ((abr-vide? ABR) #f)
      ((= valeur (abr-valeur ABR)) #t)
      ((< valeur (abr-valeur ABR)) (abr-membre? (abr-fils-gauche ABR) valeur))
      (else (abr-membre? (abr-fils-droit ABR) valeur)))))

;Retourne une liste triée des valeurs présentes dans l'ABR
(define abr-parcours-ordonne
  (lambda (ABR)
    (cond
      ((abr-vide? ABR) '())
      (else (append
              (abr-parcours-ordonne (abr-fils-gauche ABR))
              (list (abr-valeur ABR))
              (abr-parcours-ordonne (abr-fils-droit ABR)))))))

;Compte le nombre de valeurs dans l'ABR
(define abr-taille
  (lambda (ABR)
    (cond
      ((abr-vide? ABR) 0)
      (else (+ 1
               (abr-taille (abr-fils-gauche ABR))
               (abr-taille (abr-fils-droit ABR)))))))

;Display le menu dans l'interface de commande
(define (menu)
  (display "Menu\n1. Ajout noeud\n2. Supprimer noeud\n3. Recherche noeud\n4. Parcours ordonné\n5. Taille arbre\n6. Afficher arbre\n7. Réinitialiser arbre\n8. Sortir programme\n"))

;Vérifie que le numéro de commande donné par l'utilisateur existe, si ce n'est pas le cas redemande à l'utilisateur de rentrer un numéro
(define (read-command)
(display "Enter choice [1-8] >")
(let* ((in (read-line))
        (n (string->number in)))
(cond ((<= 1 n 8) n)
        (else
        (display "Invalid choice \"")
        (display in)
        (display "\"\n")
        (read-command)))))

;Controller principal, exécute les différentes commandes de l'utilisateur
(define (driver ABR)
  (menu)
  (let ((choice (read-command)))
    (cond ((= choice 1) (display "Entrer la valeur à ajouter >")
                        (driver (abr-ajout ABR (string->number (read-line)))))
          ((= choice 2) (display "Entrer la valeur à supprimer >")
                        (driver (abr-supp ABR (string->number(read-line)))))
          ((= choice 3) (display "Entrer la valeur à rechercher >")
                        (display (abr-membre? ABR (string->number(read-line))))
                        (display "\n")
                        (driver ABR))
          ((= choice 4) (display "parcours ordonné:\n\n")
                        (display (abr-parcours-ordonne ABR))
                        (display "\n")
                        (driver ABR))
          ((= choice 5) (display "Taille de l'arbre:\n\n")
                        (display (abr-taille ABR))
                        (display "\n")
                        (driver ABR))
          ((= choice 6) (display "Affichage de l'arbre\n\n")
                        (display ABR)
                        (display "\n")
                        (driver ABR))
          ((= choice 7) (display "Arbre vidé\n")
                        (driver (abr-creer-vide)))
          (else (display "Fin du programme")))))


(define (start)
  (driver (abr-creer-vide)))

