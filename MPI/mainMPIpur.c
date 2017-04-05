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
                  //break;    

                T->alpha = MAX(T->alpha, child_score);
        }

        if (TRANSPOSITION_TABLE)
          tt_store(T, result);
}

/*-----------------------------------------------------------------------------------------------------------*/

void evaluate_master_slave(tree_t *T, result_t *result, int *n_rang, int *size){
  

  int nn_rang = *n_rang;
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
  
  //n_moves = generate_legal_moves(T, &moves[0]);

  /* absence de coups légaux : pat ou mat */
  //if (n_moves == 0) {
          //result->score = check(T) ? -MAX_SCORE : CERTAIN_DRAW;
          //return;
        //}
  
  //if (ALPHA_BETA_PRUNING)
    //sort_moves(T, n_moves, moves);

/*-----------------------------------------------------------------------------------------------------------*/
  // MPI parametres

  MPI_Status status;



  //MPI_Comm_rank(MPI_COMM_WORLD,&n_rang);
  //MPI_Comm_size(MPI_COMM_WORLD, &size);

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
/* création d'un nouveau type MPI "tree_t" */

  /*MPI_Datatype MPI_tree, ancientType[14] = {MPI_CHAR, MPI_CHAR, MPI_INT, MPI_INT, MPI_INT, MPI_INT, MPI_INT, MPI_INT, MPI_INT, MPI_INT, MPI_CHAR, MPI_INT, MPI_INT, MPI_INT};
  int block[14] = {128, 128, 1, 1, 1, 1, 1, 1, 2, 2, 128, 1, 1, MAX_DEPTH};
  MPI_Aint display[14];


  display[0] = offsetof(tree_t, pieces);
  display[1] = offsetof(tree_t, colors);
  display[2] = offsetof(tree_t, side);
  display[3] = offsetof(tree_t, depth);
  display[4] = offsetof(tree_t, height);
  display[5] = offsetof(tree_t, alpha);
  display[6] = offsetof(tree_t, beta);
  display[7] = offsetof(tree_t, alpha_start);
  display[8] = offsetof(tree_t, king);
  display[9] = offsetof(tree_t, pawns);
  display[10] = offsetof(tree_t, attack);
  display[11] = offsetof(tree_t, suggested_move);
  display[12] = offsetof(tree_t, hash);
  display[13] = offsetof(tree_t, history);

  MPI_Type_create_struct(14, block, display, ancientType, &MPI_tree);
  MPI_Type_commit(&MPI_tree);*/


/*-----------------------------------------------------------------------------------------------------------*/

/* MASTER ZONE */

if(nn_rang == 0){
  
  n_moves = generate_legal_moves(T, &moves[0]);
  /* absence de coups légaux : pat ou mat */
  if (n_moves == 0) {
          result->score = check(T) ? -MAX_SCORE : CERTAIN_DRAW;
          return;
        }



  result_t resultat;

  tree_t arbre;
  int r_tache = 0;
  int n_tache = 0;


  int n_size = *size;
  for(int p = 1; p < n_size; p++){
    if(n_tache < n_moves){
      MPI_Send(&moves[n_tache], 1, MPI_INT, p, TAG_DATA, MPI_COMM_WORLD);
      n_tache++;

    }
  }

  while((r_tache < n_tache) || (n_tache < n_moves)){
      

    if(r_tache < n_tache){
      MPI_Recv(&resultat, 1, MPI_resultat, MPI_ANY_SOURCE, TAG_DATA, MPI_COMM_WORLD, &status);
      r_tache++;
    
      int child_score = -resultat.score;

      if (child_score > result->score) {
        result->score = child_score;
        result->best_move = moves[r_tache];
        result->pv_length = resultat.pv_length + 1;
          for(int j = 0; j < resultat.pv_length; j++)
            result->PV[j+1] = resultat.PV[j];
        result->PV[0] = moves[r_tache];
        
      }
    }
        /*int child_score = -child_result.score;

                if (child_score > result->score) {
                  result->score = child_score;
                  result->best_move = moves[i];
                  result->pv_length = child_result.pv_length + 1;
                    for(int j = 0; j < child_result.pv_length; j++)
                      result->PV[j+1] = child_result.PV[j];
                    result->PV[0] = moves[i];*/

      //T->alpha = MAX(T->alpha, child_score);
      //printf("T->alpha = %d", T->alpha);
    //}

    if(n_tache < n_moves){
        MPI_Send(&moves[n_tache], 1, MPI_INT, status.MPI_SOURCE, TAG_DATA, MPI_COMM_WORLD);
        n_tache++;
      }

    /* s'il ny a plus de coup c'est que le master a terminé */
    else{
      //break;
      printf("master has finished \n");
    }

  }
     for(int p = 1; p < n_size; p++){
      MPI_Send(0, 0, MPI_INT, p, TAG_END, MPI_COMM_WORLD);
    }
  
}

/*-----------------------------------------------------------------------------------------------------------*/

/* SLAVES ZONE */

if(nn_rang != 0){

  int n_tache;
  //int r_tache = 0;

  while(1/*r_tache < n_tache*/){

/* l'eclave recoit du travail */
  MPI_Recv(&moves[n_tache], 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
  //r_tache++;
/* on verifie si le tag est un TAG DATA*/
    if(status.MPI_TAG == TAG_DATA){

      tree_t arbre;
      result_t resultat; 
      
      resultat.score = -MAX_SCORE - 1;
      resultat.pv_length = 0;

      play_move(T, moves[n_tache], &arbre);

      evaluate(&arbre, &resultat);

      MPI_Send(&resultat, 1, MPI_resultat, 0, TAG_DATA, MPI_COMM_WORLD);
    
    }
  else { break;  }
  }
}

/*-----------------------------------------------------------------------------------------------------------*/

  MPI_Type_free(&MPI_resultat);
  //MPI_Type_free(&MPI_tree);

  MPI_Barrier(MPI_COMM_WORLD);
  
  }



/*-----------------------------------------------------------------------------------------------------------*/


void decide(tree_t *T, result_t *result, int *n_rang, int *size){

  int arret = 0;

  int rang;
  MPI_Comm_rank(MPI_COMM_WORLD, &rang);

  for (int depth = 1;!arret; depth++) {
    T->depth = depth;
    T->height = 0;
    T->alpha_start = T->alpha = -MAX_SCORE - 1;
    T->beta = MAX_SCORE + 1;


    if(*n_rang == 0){
      printf("=====================================\n");


      (depth > 3) ? evaluate_master_slave(T, result, n_rang, size) : evaluate(T, result);

      printf("depth: %d / score: %.2f / best_move : ", T->depth, 0.01 * result->score);
      print_pv(T, result);


    if (DEFINITIVE(result->score)){
        //printf("break break break processeur %d\n", *n_rang);
        //break;
        arret = 1;
    }

      MPI_Bcast(&arret, 1, MPI_INT, rang, MPI_COMM_WORLD);

  }

  else{
    if(depth > 3)
      evaluate_master_slave(T, result, n_rang, size);

      MPI_Bcast(&arret, 1, MPI_INT, rang, MPI_COMM_WORLD);

  }

  }
}


/*-----------------------------------------------------------------------------------------------------------*/


int main(int argc, char **argv){  


  /* les paramètres de l'environement MPI */
  int n_rang; /* le rang */
  int size; /* nombre de processus */
  MPI_Status status;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &n_rang);

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
  if(n_rang == 0)
    print_position(&root);
  
  debut = my_gettimeofday();
  
  decide(&root, &result, &n_rang, &size);

  fin = my_gettimeofday();
  printf("\nTemps total de calcul : %g seconde(s) \n", fin - debut);
  
    
  if (n_rang == 0){
    printf("\nDécision de la position: ");
          switch(result.score * (2*root.side - 1)) {
          case MAX_SCORE: printf("blanc gagne\n"); break;
          case CERTAIN_DRAW: printf("partie nulle\n"); break;
          case -MAX_SCORE: printf("noir gagne\n"); break;
          default: printf("BUG\n");
          }

          printf("Node searched: %llu\n", node_searched);
  }


        if (TRANSPOSITION_TABLE)
          free_tt();


    MPI_Finalize();

  return 0;
}