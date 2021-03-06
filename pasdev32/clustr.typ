(* CLUSTER program's type file *)

  type

        (* pointer types that point to different record nodes *)

        entity_ptr  = ^entity_node ;
        child_ptr   = ^child_node ;
        cost_ptr    = ^cost_node;
        list_ptr    = ^list_node;
        process_ptr = ^process_node;
        opt_ptr     = ^opt_node;


        (* represents a leaf in the hierarchical tree *)

        entity_node = record
            entity_num   : integer ;   (* entity identifing number *)
            srs          : integer ;   (* stored record size *)
            nr           : integer ;   (* number of record occurences *)
            ii_freq      : integer ;   (* trans freq from i to i  *)
            sibling_ptr  : entity_ptr; (* ptr to next sibling *)
            first_child  : child_ptr;  (* ptr to first child *)
            cluster_list : cost_ptr    (* ptr to list of clusterings *)
          end;


        (* links together parents and children in the tree structure *)

        child_node = record
          ij_freq    : integer;        (* trans freq from i to j *)
          ij_ptr     : entity_ptr;     (* ptr to child entity *)
          next_child : child_ptr       (* prt to next child node *)
        end;


$PAGE cluster.typ
        (* used in clustering and computing costs *)

        cost_node = record
          cost_owner     : entity_ptr; (* ptr to owner of cost node *)
          d_of_ii        : real;    (* *)
          pba            : real;       (* *)
          cost           : real;       (* *)
          join_separate  : cost_ptr;   (* nodes joined separately *)
          join_clustered : cost_ptr;   (* nodes joined clustered *)
          sep_children   : list_ptr;   (* list of seperated children *)
          clus_children  : list_ptr;   (* list of clustered children *)
          next_choice    : cost_ptr    (* ptr to next clustering in list *)
        end;


        (* holds entity numbers of a clustering list *)

        list_node = record
          entity_num     : integer;    (* child entity number *)
          sep_children   : list_ptr;   (* list of seperated children *)
          clus_children  : list_ptr;   (* list of clustered children *)
          next_kid       : list_ptr    (* next kid in the list *)
        end;


        (* used in processing all cluster combinations *)

        process_node = record
          entity      : entity_ptr;    (* ptr to child entity *)
          cur_cluster : cost_ptr;      (* current cluster *)
          next_node   : process_ptr    (* next child's node in list *)
        end;


        (* used in forming an "optimized" list of cost nodes *)

        opt_node = record
          keeper : cost_ptr;        (* ptr to a cost node to keep *)
          pred   : opt_ptr;         (* predecesor opt_node *)
          next   : opt_ptr;         (* next opt_node *)
        end;


        (* general types used in more than one module *)

        direction = ( enter_proc, exit_proc );  (* used in procedure tracing *)

        join_type = (   separate, clustered );  (* ways to join nodes *)
 