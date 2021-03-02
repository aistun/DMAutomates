## Run d'un automate top-down

Un run d'un automate d'arbre $\mathcal{A} = (Q, \Sigma, \delta, I)$ pour un arbre $t \in \mathcal{T}(\Sigma)$ est une fonction $r : dom(t) \to Q$ telle que $\forall p \in dom(t), (r(p1), ..., r(pk)) \in \delta(r(p), t(p))$, avec $|t(p)| = k$. Un run est dit acceptant si et seulement si $r(\varepsilon) \in I$.

## 2. Validation top-down non-déterministe

1. Un run d'un automate d'arbre $A = (Q, \Sigma, \delta, I, F)$ pour un arbre $t \in \mathcal{T}(\Sigma)$ est une fonction $r : dom(t) \to Q$ telle que $\forall p \in dom(t), (r(p1), ..., r(pk)) \in \delta(L, r(p))$ et $t(p) \in L$, avec $|t(p)| = k$. Un run est dit acceptant si et seulement si $r(\varepsilon) \in I$ et $\forall p \in dom(t), |t(p)| = 0 \to r(p) \in F$.

2. ```
   validate_td(automate A, arbre t, chemin p, état q):
   	si t(p) = #:
		rendre (t(p) est dans A.F)
   	pour toute transition (L, q', q1, q2) dans A:
   		si q = q' et t(p) est dans L:
   			r1 <- validate_td(A, t, p1, q1)
   			r2 <- validate_td(A, t, p2, q2)
   			si r1 et r2:
   				rendre vrai
   	rendre faux
   ```
   
3. - $\exists q \in I$ : complexité $O(|I|)$.
   - validate_td(A, t, $\varepsilon$, q) :
     - pour toute transition (L, q', q1, q2) dans A : $O(|A|)$.
     - t(p) est dans L : comme $L \in \mathcal{P}_f(\Sigma) \cup \mathcal{P}_{cof}(\Sigma)$ alors le test d'appartenance se fait en temps fini, on va considérer qu'il se fait en temps $O(1)$.
     - dans le pire des cas, on va faire $\underbrace{|A| \times ... \times |A|}_{\max_{p \in dom(t)} |p|}$ opérations.
     - et dans le pire des cas (où $t$ est un arbre filaire) $max_{t \in dom(t)} |p| = |t|$
   - donc complexité totale : $O(|I| \times |A|^{|t|})$.

## Run d'un automate bottom-up

Un run d'un automate d'arbre $\mathcal{A} = (Q, \Sigma, \delta, F)$ pour un arbre $t \in \mathcal{T}(\Sigma)$ est une fonction $r : dom(t) \to Q$ telle que $\forall p \in dom(t), r(p) \in \delta(t(p)(r(p1), ..., r(pk)))$, avec $|t(p)| = k$. Un run est dit acceptant si et seulement si $r(\varepsilon) \in F$.

## 3. Validation bottom-up

Un run d'un automate d'arbre $\mathcal{A} = (Q, \Sigma, \delta, I, F)$ pour un arbre $t \in \mathcal{T}(\Sigma)$ est une fonction $r : dom(t) \to Q$ telle que $\forall p \in dom(t), \forall L, t(p) \in L \text{ et } (L, r(p)) \in \delta^{-1}(p1, ..., pk)$, avec $|t(p)| = k$. Un run est dit acceptant si et seulement si $r(\varepsilon) \in F$.

1. ```
   validate_bu(automate A, arbre t, chemin p):
   	si t(p) = #:
   		R <- {}
   		pour toute transition (L, q, _, _) dans A:
   			si # est dans L:
   				ajouter q à R
   		rendre R
   	sinon:
           R1 <- validate_bu(A, t, p1)
           R2 <- validate_bu(A, t, p2)
           R  <- {}
           pour toute transition (L, q, q1, q2) dans A:
               si q1 est dans R1 et q2 est dans R2 et t(p) est dans L:
                   ajouter q à R
           rendre R
   ```

2. - avec les appels récursifs, les calculs des lignes 5 à 8 vont être exécutées pour tous les noeuds de l'arbre, donc $|t|$ fois.
   - à chaque fois, ces calculs se font en temps $O(|A| \times |R_1| \times |R_2|)$.
   - on peut supposer que $|R_1| = O(|A|)$ et $|R_2| = O(|A|)$ car à chaque fois les ensembles $R$ sont construits à partir des transitions.
   - donc complexité totale validate_bu(A, t, $\varepsilon$) : $O(|t||A|^3)$.
   - et intersection $\texttt{validate_bu}(A, t, \varepsilon) \cap I$ : complexité $O(|\texttt{validate_bu}(A, t, \varepsilon)| \times |I|) = O(|A|^2)$.
   - donc complexité totale : $O(|t||A|^3 + |A|^2) = O(|t||A|^3)$.

3. ```
   validate_opt(automate A, arbre t, chemin p, ensemble de candidats P):
   	si |P| = 0:
   		rendre {}
   	si t(p) = #:
   		R <- {}
   		pour toute transition (L, q, _, _) dans A:
   			si q est dans P et # est dans L:
               	ajouter q à R
   		rendre R
   	sinon:
   		P'  <- {}
   		P1  <- {}
   		P2  <- {}
   		pour toute transition (L, q, q1, q2) dans A:
   			si q est dans P et t(p) est dans L:
   				ajouter q  à P'
   				ajouter q1 à P1
   				ajouter q2 à P2
   
           R1 <- validate_opt(A, t, p1, P1)
           R2 <- validate_opt(A, t, p2, P2)
           si |R1| = 0 ou |R2| = 0:
           	rendre {}
          	
           R  <- {}
           pour toute transition (L, q, q1, q2) dans A:
               si q est dans P' et q1 est dans R1 et q2 est dans R2:
                   ajouter q à R
           rendre R
   ```

   Si $P$ est vide, alors la fonction doit immédiatement rendre $\emptyset$.

   Si $R$ est vide, dans le cas où il provient d'un appel récursif (il s'agit donc d'un $R_1$ ou $R_2$) alors nécessairement $|R| = 0$, donc on fait remonter ce résultat sans faire aucun calculs (sauf celui des ensembles d'états candidats $P_1$ et $P_2$). On a donc que si dans l'un des appels récursifs renvoie $\emptyset$, alors le résultat de l'appel $\texttt{validate_opt}(A, t, p, P)$ est $\emptyset$. (A REVOIR ET PRÉCISER)

## 4. Compilation

1. ```
   compile_label(label l):
   	match l:
   	| mot m : rendre {m}
   	| *     : rendre \Sigma \ {}
   	| ~l1~...~ln :
   		compiler l1, ..., ln en e1, ..., en
   		a <- faux
   		b <- faux
   		s'il y a des ensembles ei de la forme ei = {...}:
   			a <- vrai
   			fusionner tous les ensembles ei tels que ei = {...} 
   				en un seul ensemble A = {...}
   		s'il y a des ensembles ei de la forme ei = \Sigma \ {...}:
   			b <- vrai
   			fusionner tous les ensembles ei tels que ei = \Sigma \ {...}
   				en un seul ensemble \Sigma \ B où B = {...}
   		si a et b:
   			rendre B \ (B \cap A)
   		si a:
   			rendre \Sigma \ A
   		si b:
   			rendre B
   ```

   ```
   compilation_types(ensemble de types T):
   	Q <- {} (* ensemble vide des états *)
   	L <- {} (* map vide *)
   	T <- {} (* ensemble vide des transitions *)
   	pour chaque type t = l[r]:
   		créer un état qt et Q <- Q U {qt}
   		lt <- compile_label(l)
   		L  <- ajouter (t -> lt) à L
   	pour chaque définition de type t = l[r]:
   		(Qr, Ir, Fr, Tr) <- NFA de l'expression régulière r avec qt comme état initial
   		Q <- Q U Qr
   		pour chaque transition q -a-> q' de l'automate de r:
   			si q' n'est pas dans Fr ou si q = q':
   				ajouter la transition L[a], q -> qa, q' à T
   			sinon:
               	ajouter la transition L[a], q -> qa, q# à T
       I = { qt | si t peut être un type de la racine }
   	rendre (Q, I, T)
   ```

   Pour la compilation de l'expression régulière r avec qt comme état initial en un NFA on utilise la construction de Glushkov.

2. - définition de type : t = (l, r):
     - taille de l, inductivement:
       - mot $m$ -> $1$
       - $*$      -> $1$
       - $\sim l_1 \sim ... \sim l_n$ -> $\sum_{i = 1}^{n} |l_i|$
     - taille de r, inductivement:
       - type $t$ -> $1$
       - $r*$     -> $1 + |r|$
       - $r?$     -> $1 + |r|$
       - $r_1 r_2$   -> $|r_1| + |r_2|$
       - $r_1|r_2$   -> $|r_1| + |r_2|$
   - taille d'un label compilé, inductivement :
     - mot $m$ -> $|\{1\}| = 1$
     - $*$      -> $|\emptyset| = 0$
     - $\sim l_1 \sim ... \sim l_n$ -> 
       - notons $e_1, ..., e_n$ les ensembles compilés des labels $l_1, ..., l_n$, les étapes de fusions donnent deux ensembles $A$ et $B$ qui sont tels que $|A| + |B| = \sum_{i = 1}^{n} |e_i|$.
       - 
