#include "projet.h"

#include <sys/time.h>   /* chronometrage */
#include <mpi.h>


#define TAG_BEGIN   1
#define TAG_DATA    2
#define TAG_RESULT  4
#define TAG_END     8

/* 2017-02-23 : version 1.0 */

double my_gettimeofday(){
  struct timeval tmp_time;
  gettimeofday(&tmp_time, NULL);
  return tmp_time.tv_sec + (tmp_time.tv_usec * 1.0e-6L);
}


unsigned long long int node_searched = 0;

void evaluate(tree_t *T, result_t *result){
  
        node_searched++;
  
        move_t moves[MAX_MOVES];
        int n_moves;

        result->score = -MAX_SCORE - 1;
        result->pv_length = 0;
        
        if (test_draw_or_victory(T, result))
          return;

        //if (TRANSPOSITION_TABLE && tt_lookup(T, result))     /* la réponse est-elle déjà connue ? */
          //return;
        
        compute_attack_squares(T);

        /* profondeur max atteinte ? si oui, évaluation heuristique */
        if (T->depth == 0) {
          result->score = (2 * T->side - 1) * heuristic_evaluation(T);
          return;
        }
        
        n_moves = generate_legal_moves(T, &moves[0]);

        /* absence de coups légaux : pat ou mat */
       if (n_moves == 0) {
          result->score = check(T) ? -MAX_SCORE : CERTAIN_DRAW;
          return;
        }
        
        if (ALPHA_BETA_PRUNING)
          sort_moves(T, n_moves, moves);

        /* évalue récursivement les positions accessibles à partir d'ici */
        for (int i = 0; i < n_moves; i++) {
                tree_t child;
                result_t child_result;
                
                play_move(T, moves[i], &child);
                
                evaluate(&child, &child_result);
                         
                int child_score = -child_result.score;

                if (child_score > result->score) {
                  result->score = child_score;
                  result->best_move = moves[i];
                  result->pv_length = child_result.pv_length + 1;
                    for(int j = 0; j < child_result.pv_length; j++)
                      result->PV[j+1] = child_result.PV[j];
                    result->PV[0] = moves[i];
              }

                if (ALPHA_BETA_PRUNING && child_score >= T->beta)
                  break;    

                T->alpha = MAX(T->alpha, child_score);
        }

        if (TRANSPOSITION_TABLE)
          tt_store(T, result);
}



/*-----------------------------------------------------------------------------------------------------------*/

void evaluate_master_slave(tree_t *T, result_t *result, int rang, int size){
  
  node_searched++;



  move_t moves[MAX_MOVES];
  int n_moves;

  result->score = -MAX_SCORE - 1;
  result->pv_length = 0;

  
  if (test_draw_or_victory(T, result))
    return;

  if (TRANSPOSITION_TABLE && tt_lookup(T, result))     /* la réponse est-elle déjà connue ? */
    return;
  
  compute_attack_squares(T);

  /* profondeur max atteinte ? si oui, évaluation heuristique */
  if (T->depth == 0) {
    result->score = (2 * T->side - 1) * heuristic_evaluation(T);
    return;
  }
  

/*-----------------------------------------------------------------------------------------------------------*/
  // MPI parametres

  MPI_Status status;
/*-----------------------------------------------------------------------------------------------------------*/
/* création d'un nouveau type MPI "result_t" */
  MPI_Datatype MPI_resultat, ancienType[4] = {MPI_INT, MPI_INT, MPI_INT, MPI_INT};
  int bloc[4] = {1, 1, 1, MAX_DEPTH};
  MPI_Aint disp[4];

  disp[0] = offsetof(result_t, score);
  disp[1] = offsetof(result_t, best_move);
  disp[2] = offsetof(result_t, pv_length);
  disp[3] = offsetof(result_t, PV);

  MPI_Type_create_struct(4, bloc, disp, ancienType, &MPI_resultat);
  MPI_Type_commit(&MPI_resultat);

/*-----------------------------------------------------------------------------------------------------------*/

n_moves = generate_legal_moves(T, &moves[0]);
  /* absence de coups légaux : pat ou mat */
  if (n_moves == 0) {
          result->score = check(T) ? -MAX_SCORE : CERTAIN_DRAW;
          return;
        }

  if (ALPHA_BETA_PRUNING)
    sort_moves(T, n_moves, moves);


/*-----------------------------------------------------------------------------------------------------------*/

/* MASTER ZONE */

if(rang == 0){

  result_t resultat;

  int r_tache = 0;
  int n_tache = 0;


  int taches[n_moves];

  for(int p = 1; p < size; p++){
    if(n_tache < n_moves){
      MPI_Send(&n_tache, 1, MPI_INT, p, TAG_DATA, MPI_COMM_WORLD);
      taches[p]=n_tache;
      n_tache++;
    }
  }



  while((r_tache < n_tache) || (n_tache < n_moves)){

    if(r_tache < n_tache){
      MPI_Recv(&resultat, 1, MPI_resultat, MPI_ANY_SOURCE, TAG_DATA, MPI_COMM_WORLD, &status);

      int child_score = -resultat.score;

      if (child_score > result->score) {
        result->score = child_score;
        result->best_move = moves[taches[status.MPI_SOURCE]];

        result->pv_length = resultat.pv_length + 1;
        for(int j = 0; j < resultat.pv_length; j++)
          result->PV[j+1] = resultat.PV[j];
        result->PV[0] = moves[taches[status.MPI_SOURCE]];

      }
      r_tache++;

      if (ALPHA_BETA_PRUNING && child_score >= T->beta)
                  break;    

         T->alpha = MAX(T->alpha, child_score);
    }

    if(n_tache < n_moves){
        MPI_Send(&n_tache, 1, MPI_INT, status.MPI_SOURCE, TAG_DATA, MPI_COMM_WORLD);
        n_tache++;
      }

  }
     for(int p = 1; p < size; p++){
      MPI_Send(0, 0, MPI_INT, p, TAG_END, MPI_COMM_WORLD);
    }
}

/*-----------------------------------------------------------------------------------------------------------*/

/* SLAVES ZONE */

if(rang != 0){

  int n_tache;
  
    while(1){
    MPI_Recv(&n_tache, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

    if(status.MPI_TAG != TAG_END){
    /* on verifie si le tag est un TAG DATA*/

          tree_t arbre;
          result_t resultat; 
          
          resultat.score = -MAX_SCORE - 1;
          resultat.pv_length = 0;


          play_move(T, moves[n_tache], &arbre);

          evaluate(&arbre, &resultat);

          MPI_Send(&resultat, 1, MPI_resultat, 0, TAG_DATA, MPI_COMM_WORLD);

        
        }

      else {    break;  }
    }
  
}

/*-----------------------------------------------------------------------------------------------------------*/

  MPI_Type_free(&MPI_resultat);
  
  }

/*-----------------------------------------------------------------------------------------------------------*/


void decide(tree_t *T, result_t *result, int rang, int size){

  int arret = 0;


  for (int depth = 1;!arret; depth++) {
    T->depth = depth;
    T->height = 0;
    T->alpha_start = T->alpha = -MAX_SCORE - 1;
    T->beta = MAX_SCORE + 1;

    if(rang == 0){
      printf("=====================================\n");


      (depth > 2) ? evaluate_master_slave(T, result, rang, size) : evaluate(T, result);
      evaluate_master_slave(T, result, rang, size);

      printf("depth: %d / score: %.2f / best_move : ", T->depth, 0.01 * result->score);
      print_pv(T, result);


    if (DEFINITIVE(result->score)){
        //break;
        arret = 1;
      }

      MPI_Bcast(&arret, 1, MPI_INT, 0, MPI_COMM_WORLD);

  }

  else{
    if(depth > 2)
      evaluate_master_slave(T, result, rang, size);

      MPI_Bcast(&arret, 1, MPI_INT, 0, MPI_COMM_WORLD);
    }

  }
}


/*-----------------------------------------------------------------------------------------------------------*/


int main(int argc, char **argv){  


  /* les paramètres de l'environement MPI */
  int size; /* nombre de processus */
  MPI_Status status;
  int rang ; /* le rang */
  
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rang);

  tree_t root;
  result_t result;

  double debut, fin;

  if (argc < 2) {
    printf("usage: %s \"4k//4K/4P w\" (or any position in FEN)\n", argv[0]);
    exit(1);
  }

  if (ALPHA_BETA_PRUNING)
    printf("Alpha-beta pruning ENABLED\n");

  if (TRANSPOSITION_TABLE) {
    printf("Transposition table ENABLED\n");
    init_tt();
  }
  
  parse_FEN(argv[1], &root);
  if(rang == 0)
    print_position(&root);
  
  debut = my_gettimeofday();
  
  decide(&root, &result, rang, size);

  fin = my_gettimeofday();

  if(rang == 0)
  printf("\nTemps total de calcul : %g seconde(s) \n", fin - debut);
  

  if (rang == 0){
    unsigned long long int node = 0;
    MPI_Reduce(&node_searched, &node, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    
    printf("\nDécision de la position: ");
          switch(result.score * (2*root.side - 1)) {
          case MAX_SCORE: printf("blanc gagne\n"); break;
          case CERTAIN_DRAW: printf("partie nulle\n"); break;
          case -MAX_SCORE: printf("noir gagne\n"); break;
          default: printf("BUG\n");
          }

          printf("Node searched: %llu\n", node);
  }
  else {
        MPI_Reduce(&node_searched, NULL, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

  }


  if (TRANSPOSITION_TABLE)
    free_tt();


    MPI_Finalize();

  return 0;
}
