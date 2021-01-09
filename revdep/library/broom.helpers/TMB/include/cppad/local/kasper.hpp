CppAD::vector<tape_point> tp_;    /* Vector of tape entries */
CppAD::vector<size_t> var2op_;    /* Get operator (index) that produced the given var 
				     as result */
CppAD::vector<size_t> op_mark_;   /* Mark operators that must be computed */
CppAD::vector<CppAD::vector<int> > colpattern;
CppAD::vector<bool> arg_mark_;    /* op_arg_rec_ contains argument pointers.
				     Need vector of the same length (op_arg_rec_.size()) 
				     containing bool. Call this vector "arg_mark_". 
				     Need to be able to lookup "arg_mark_[arg]" where arg 
				     is addr_t*.
				     We have to first convert pointer to index:  
				       index=addr_t(op_arg-op_arg_rec_.data());
				     Then do the lookup arg_mark_[index];
				  */
CppAD::vector<bool> user_region_; /* Vector of same length as tp_ (tape_points) for which 
				     user_region[i] is true iff tp_[i].op is either "UserOp"
				     or one of "Usr**" operators.
				  */
CppAD::vector<size_t> user_region_mark_;  /* user_region_mark_[i] is marked if the i'th tape
					   point belongs to an already marked user region. */
CppAD::vector<bool> constant_tape_point_; /* Vector of same length as tp_ (tape_points) that 
					     marks all tape_points that only depend on fixed 
					     effects. */
/* Helper function to print std::vector */
template<typename T>
void vprint(const std::vector<T>& v) {
    Rcout << "[";
    size_t last = v.size() - 1;
    for(size_t i = 0; i < v.size(); ++i) {
        Rcout << v[i];
        if (i != last)
            Rcout << ", ";
    }
    Rcout << "]";
    Rcout << "\n";
}
/* Helper function for markArgs */
void markOpField(
		 //std::ostream      &os ,
		 void* os,
		 const char *leader,
		 //const Type     &value, 
		 const addr_t*   op_arg,
		 size_t          width ){
  addr_t index=addr_t(op_arg-play_.op_arg_rec_.data());
  arg_mark_[index]=true;
}
bool isDepArg(const addr_t*   op_arg){
  addr_t index=addr_t(op_arg-play_.op_arg_rec_.data());
  return   arg_mark_[index];
}
void mark_tape_point_args(size_t index, size_t mark){
  tape_point tp1=tp_[index];
  tape_point tp2=tp_[index+1];
  const addr_t* op_arg;
  op_arg=tp1.op_arg;
  int numarg=tp2.op_arg - op_arg;
  for(int i=0;i<numarg;i++){
    if(isDepArg(&op_arg[i]))op_mark_[var2op_[op_arg[i]]]=mark;
  }
}
/* Mark all relevant arguments for a given operator.
   Copy-pasted "printOp" from "op_code.hpp" and modified... 
   Only run once for each operator index.
*/
void markArgs(tape_point &tp)
{
  OpCode                 op=tp.op;
  const addr_t          *ind=tp.op_arg;    
  
  size_t i;

  void *os; //,*Rec,*i_var,*nfz,*fz,*nrz,*rz;
  os=NULL;
	
	// print operator
	// markOpField(os,  "i=",      i_var, 5);
	// if( op == CExpOp )
	// {	markOpField(os, "op=", OpName[op], 4); 
	// 	markOpField(os, "", CompareOpName[ ind[0] ], 3);
	// }
	// else if( op == ComOp )
	// {	markOpField(os, "op=", OpName[op], 3); 
	// 	markOpField(os, "", CompareOpName[ ind[0] ], 4);
	// }
	// else	markOpField(os, "op=", OpName[op], 7); 

	// print other fields
	size_t ncol = 5;
	switch( op )
	{
		case CSumOp:
		/*
		ind[0] = number of addition variables in summation
		ind[1] = number of subtraction variables in summation
		ind[2] = index of parameter that initializes summation
		ind[3], ... , ind[2+ind[0]] = index for positive variables
		ind[3+ind[0]], ..., ind[2+ind[0]+ind[1]] = negative variables 
		ind[3+ind[0]+ind[1]] = ind[0] + ind[1]
		*/
		CPPAD_ASSERT_UNKNOWN( ind[3+ind[0]+ind[1]] == ind[0]+ind[1] );
		//markOpField(os, " pr=", Rec->GetPar(ind[2]), ncol);
		for(i = 0; i < ind[0]; i++)
			 markOpField(os, " +v=", &ind[3+i], ncol);
		for(i = 0; i < ind[1]; i++)
			 markOpField(os, " -v=", &ind[3+ind[0]+i], ncol);
		break;

		case LdpOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 3 );
		markOpField(os, "off=", &ind[0], ncol);
		markOpField(os, "idx=", &ind[1], ncol);
		break;

		case LdvOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 3 );
		markOpField(os, "off=", &ind[0], ncol);
		markOpField(os, "  v=", &ind[1], ncol);
		break;

		case StppOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 3 );
		markOpField(os, "off=", &ind[0], ncol);
		markOpField(os, "idx=", &ind[1], ncol);
		//markOpField(os, " pr=", Rec->GetPar(ind[2]), ncol);
		break;

		case StpvOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 3 );
		markOpField(os, "off=", &ind[0], ncol);
		markOpField(os, "idx=", &ind[1], ncol);
		markOpField(os, " vr=", &ind[2], ncol);
		break;

		case StvpOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 3 );
		markOpField(os, "off=", &ind[0], ncol);
		markOpField(os, " vl=", &ind[1], ncol);
		//markOpField(os, " pr=", Rec->GetPar(ind[2]), ncol);
		break;

		case StvvOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 3 );
		markOpField(os, "off=", &ind[0], ncol);
		markOpField(os, " vl=", &ind[1], ncol);
		markOpField(os, " vr=", &ind[2], ncol);
		break;

		case AddvvOp:
		case DivvvOp:
		case LevvOp:
		case LtvvOp:
		case EqvvOp:
		case NevvOp:
		case MulvvOp:
		case PowvvOp:
		case SubvvOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 2 );
		markOpField(os, " vl=", &ind[0], ncol);
		markOpField(os, " vr=", &ind[1], ncol);
		break;

		case AddpvOp:
		case LepvOp:
		case LtpvOp:
		case EqpvOp:
		case NepvOp:
		case SubpvOp:
		case MulpvOp:
		case PowpvOp:
		case DivpvOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 2 );
		//markOpField(os, " pl=", Rec->GetPar(ind[0]), ncol);
		markOpField(os, " vr=", &ind[1], ncol);
		break;

		case DivvpOp:
		case LevpOp:
		case LtvpOp:
		case PowvpOp:
		case SubvpOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 2 );
		markOpField(os, " vl=", &ind[0], ncol);
		//markOpField(os, " pr=", Rec->GetPar(ind[1]), ncol);
		break;

		case AbsOp:
		case AcosOp:
		case AsinOp:
		case AtanOp:
		case CosOp:
		case CoshOp:
		case ExpOp:
		case LogOp:
		case SignOp:
		case SinOp:
		case SinhOp:
		case SqrtOp:
		case UsravOp:
		case TanhOp:
		case TanOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 1 );
		markOpField(os, "  v=", &ind[0], ncol);
		break;

		case ErfOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 3 );
		// ind[2] points to the parameter 0
		// ind[3] points to the parameter 2 / sqrt(pi)
		markOpField(os, "  v=", &ind[0], ncol);
		break;

		case ParOp:
		case UsrapOp:
		case UsrrpOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 1 );
		//markOpField(os, "  p=", Rec->GetPar(ind[0]), ncol);
		break;

		case UserOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 4 );
		{
		  // const char* name = user_atomic<Base>::name(ind[0]);
		  // markOpField(os, " f=",   name, ncol);
		  // markOpField(os, " i=", &ind[1], ncol);
		  // markOpField(os, " n=", &ind[2], ncol);
		  // markOpField(os, " m=", &ind[3], ncol);		  

		}
		break;

		case PriOp:
		CPPAD_ASSERT_NARG_NRES(op, 5, 0);
		if( ind[0] & 1 )
			markOpField(os, " v=", &ind[1], ncol);
		//else	markOpField(os, " p=", Rec->GetPar(ind[1]), ncol);
		//os << "before=\"" << Rec->GetTxt(ind[2]) << "\"";
		if( ind[0] & 2 )
			markOpField(os, " v=", &ind[3], ncol);
		//else	markOpField(os, " p=", Rec->GetPar(ind[3]), ncol);
		//os << "after=\"" << Rec->GetTxt(ind[4]) << "\"";
		break;

		case BeginOp:
		case EndOp:
		case InvOp:
		case UsrrvOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 0 );
		break;

		case DisOp:
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 2 );
		{	//const char* name = discrete<Base>::name(ind[0]);
			//markOpField(os, " f=", name, ncol);
			markOpField(os, " x=", &ind[1], ncol);
		}
		break;
	

		case CExpOp:
		CPPAD_ASSERT_UNKNOWN(ind[1] != 0);
		CPPAD_ASSERT_UNKNOWN( NumArg(op) == 6 );
		if( ind[1] & 1 )
			markOpField(os, " vl=", &ind[2], ncol);
		//else	markOpField(os, " pl=", Rec->GetPar(ind[2]), ncol);
		if( ind[1] & 2 )
			markOpField(os, " vr=", &ind[3], ncol);
		//else	markOpField(os, " pr=", Rec->GetPar(ind[3]), ncol);
		if( ind[1] & 4 )
			markOpField(os, " vt=", &ind[4], ncol);
		//else	markOpField(os, " pt=", Rec->GetPar(ind[4]), ncol);
		if( ind[1] & 8 )
			markOpField(os, " vf=", &ind[5], ncol);
		//else	markOpField(os, " pf=", Rec->GetPar(ind[5]), ncol);
		break;


		default:
		CPPAD_ASSERT_UNKNOWN(0);
	}
	// size_t k;
	// if( NumRes(op) > 0 && (op != BeginOp) )
	// { 
	// 	for(k = 0; k < nfz; k++)
	// 		Rcout << "| fz[" << k << "]=" << fz[k];
	// 	for(k = 0; k < nrz; k++)
	// 		Rcout << "| rz[" << k << "]=" << rz[k];
	// }
	// Rcout << std::endl;
}

void my_next_reverse(
		     OpCode& op, const addr_t*& op_arg, size_t& op_index, size_t& var_index,
		     //vector<size_t>& op_mark,vector<size_t>& var2op,vector<tape_point>& tp, 
		     size_t mark)
{	
  // Call reverse_next until reach first relevant operator. First call will have op=EndOp.
  op_index--;
  while((op_mark_[op_index]!=mark) & (op_index>0))op_index--;
  // Set output values corresponding to the found operator
  op        = tp_[op_index].op;
  op_arg    = tp_[op_index].op_arg;
  op_index  = tp_[op_index].op_index;
  var_index = tp_[op_index].var_index;
  // op is marked - update dependencies
  mark_tape_point_args(op_index,mark);
  
}


/*
  mark_tape_point_args_index
  ==========================
  Given a tape_point index.
  1. Find all the arguments of this tape_point... 
  2. And, for each argument find the operators that created the argument (variable)...
  3. And mark these operators.
  Note:
  New marked operators are placed on a stack (op_mark_index_) and are thus guarantied 
  to be visited, along with children, grand children, etc. no matter which order we perform
  the search.
  We avoid placing the same node twice by maintaining a table of marked nodes (op_mark_) 
  that quickly answers whether a given node is already in the stack.


  mark_user_tape_point_index
  ==========================
  One case is special: The UserOp, and all the codes that can come in between a 
  "UserOp begin" and a "UserOp end", i.e. "Usr**". Whenever a "Usr**" is met we should 
  mark everything between the two closest "UserOp begin" and "UserOp end".
  
*/

std::vector<size_t> op_mark_index_;
void mark_user_tape_point_index(size_t index, size_t mark){
  if(user_region_mark_[index]!=mark){ /* is region already marked ? */
    tape_point tp=tp_[index];
    if(tp.op != UserOp){
      /* Find nearest UserOp tags (begin/end) */
      int i,j;
      i=index;while(tp_[i].op != UserOp)i--;
      j=index;while(tp_[j].op != UserOp)j++;
      /* Mark everything within UserOp begin/end tags - if not already marked */
      for(int k=i;k<=j;k++){
	user_region_mark_[k]=mark;
	if(op_mark_[k]!=mark){ /* FIXME: Many calls with FALSE for *all* k - why? */
	  op_mark_[k]=mark;
	  op_mark_index_.push_back(k);
	}
      }
    }
  }
}

void mark_tape_point_args_index(size_t index, size_t mark){
  tape_point tp1=tp_[index];
  tape_point tp2=tp_[index+1];
  const addr_t* op_arg;
  op_arg=tp1.op_arg;
  int numarg=tp2.op_arg - op_arg;
  for(int i=0;i<numarg;i++){
    if(isDepArg(&op_arg[i])){
      if(op_mark_[var2op_[op_arg[i]]]!=mark){ // Not already marked
	if(!constant_tape_point_[var2op_[op_arg[i]]]){ // Not constant
	  op_mark_[var2op_[op_arg[i]]]=mark;
	  op_mark_index_.push_back(var2op_[op_arg[i]]);
	}
      }
    }
  }
}
void prepare_reverse_sweep(int col){ /* input: range component */
  //Rcout << "col: " << col << " "; Rcout.flush();
  OpCode op; 
  const addr_t* op_arg;
  size_t op_index;
  size_t var_index;
  size_t dep_var_taddr=dep_taddr_[col];
  size_t mark=col+1; /*OLD: mark=dep_var_taddr; not unique! */
  op_index=var2op_[dep_var_taddr];
  op_mark_[op_index]=mark;
  /* prepare list of integers */ 
  op_mark_index_.clear();
  op_mark_index_.push_back(op_index);
  /* depth first search of operator indices */
  play_.reverse_start(op, op_arg, op_index, var_index);
  for(size_t i=0;i<op_mark_index_.size();i++){ /* Note - op_mark_index_.size() change 
						  when loop runs ...*/
    if(!constant_tape_point_[op_mark_index_[i]]){
      // If op is within user atomic region, then mark entire region. 
      if(user_region_[op_mark_index_[i]]){ /* FIXME: only do once per region !!! - Fixed! */
	mark_user_tape_point_index(op_mark_index_[i],mark); /* Appends elements to 
							       op_mark_index_ */
      }
      // op is marked - update dependencies
      mark_tape_point_args_index(op_mark_index_[i],mark); /* Appends elements to 
							     op_mark_index_ */
    }
  }
  std::sort(op_mark_index_.begin(),op_mark_index_.end());
}

void my_pattern(int col){
  //OpCode op;
  //const addr_t* op_arg;
  //size_t op_index;
  //size_t var_index;
  // size_t dep_var_taddr=dep_taddr_[col];
  // size_t mark=dep_var_taddr;
  // op_mark_[var2op_[dep_var_taddr]]=mark;
  prepare_reverse_sweep(col);
  // We always have ind_taddr_[i]=i+1
  size_t n=Domain();
  int sum=0;
  std::vector<size_t>::iterator it;
  for(it=op_mark_index_.begin();*it<=n;it++)sum++;
  colpattern[col].resize(sum);
  sum=0;
  for(it=op_mark_index_.begin();*it<=n;it++){colpattern[col][sum]=*it-1;sum++;}
}

void printTP(tape_point tp){
  Base dummy[1000];
  OpCode op=tp.op;
  const addr_t* arg=tp.op_arg;
  const Base*  Z_tmp  =dummy;//= Taylor + i_var * J;
  const Base*  pZ_tmp =dummy;//  Partial + i_var * K;
  printOp(
	  Rcout,
	  &play_,
	  tp.op_index,
	  tp.var_index,
	  op,
	  arg);
  Rcout << "\n";
}

bool is_tape_point_constant(size_t index){
  bool ok_index= (index<=tp_.size()-2);
  if(!ok_index) return false;
  tape_point tp1=tp_[index];
  tape_point tp2=tp_[index+1];
  const addr_t* op_arg;
  op_arg=tp1.op_arg;
  int numarg=tp2.op_arg - op_arg;
  // Handle the user operator special case
  if(tp1.op == UsrrvOp || tp1.op == UsrrpOp){ // Result of user atomic operation
    bool constant=true;
    size_t i=index;
    while(tp_[i].op != UserOp){
      i--;
      constant = constant && constant_tape_point_[i];
      if(tp_[i].op == UsrrvOp || tp_[i].op == UsrrpOp)break;
    }
    return constant;
  }
  if(numarg==0)return false; // E.g. begin or end operators
  bool ans=true;
  int from = 0;
  if (tp1.op == CSumOp) {
    from = 3;                // Skip arg[0], arg[1], arg[2]
  }
  for(int i=from; i<numarg; i++){
    ans = ans && ( (!isDepArg(&op_arg[i])) || constant_tape_point_[var2op_[op_arg[i]]] )   ;
  }
  return ans;
}

pod_vector<Base> Partial;
void my_init(vector<bool> keepcol){
  Partial.extend(num_var_tape_ * 1);
  arg_mark_.resize(play_.op_arg_rec_.size());
  for(size_t i=0;i<arg_mark_.size();i++)arg_mark_[i]=false;
  /* Run a reverse test-sweep to store pointers once */
  tape_point tp;
  play_.reverse_start(tp.op, tp.op_arg, tp.op_index, tp.var_index);
  tp_.resize(tp.op_index+1);
  var2op_.resize(tp.var_index+1);
  op_mark_.resize(tp.op_index+1);
  for(size_t i=0;i<op_mark_.size();i++)op_mark_[i]=0;
  user_region_mark_.resize(tp.op_index+1);
  for(size_t i=0;i<user_region_mark_.size();i++)user_region_mark_[i]=0;
  tp_[tp.op_index]=tp;
  /* 1. We need to be able to find out, for a given variable, what operator created 
     the variable. This is easiest done by looping through the _operators_ because for a 
     given op we have access to all the resulting variables it creates.
     2. We precompute the a vector of "tape_points" so that instead of calling 
     "reverse_next", we simply get the next tape entry by tp_[i-1].
  */
  while(tp.op != BeginOp ){ /* tp.op_index is decremented by one in each iteration ... */
    // printTP(tp); /* For debugging */
    play_.reverse_next(tp.op, tp.op_arg, tp.op_index, tp.var_index);
    /* Csum is special case - see remarks in player.hpp and reverse_sweep.hpp */
    if(tp.op == CSumOp)play_.reverse_csum(tp.op, tp.op_arg, tp.op_index, tp.var_index);
    for(size_t i=0;i<NumRes(tp.op);i++)var2op_[tp.var_index-i]=tp.op_index;
    tp_[tp.op_index]=tp;
    markArgs(tp);
  }
  /* Lookup table: is tape_point within a UserOp region? */
  bool user_within=false;
  user_region_.resize(tp_.size());
  for(size_t i=0;i<tp_.size();i++){
    if(tp_[i].op==UserOp){
      user_region_[i]=true;
      user_within=!user_within;	
    } else {
      user_region_[i]=user_within;
    }
  }

  /* Lookup table: is tape_point a constant (=only fixed effect dependent) ? */
  constant_tape_point_.resize(tp_.size());
  int indep_var_number=0;
  for(size_t i=0;i<tp_.size();i++){
    if(tp_[i].op==InvOp){ /* All independent variables are marked according to being
			     random or fixed effect */
      constant_tape_point_[i]=!keepcol[indep_var_number];
      indep_var_number++;
    } else { /* Mark operator as constant if _all_ arguments are constant */
      constant_tape_point_[i] = is_tape_point_constant(i);
    }

    //Rcout << constant_tape_point_[i] << " "; printTP(tp_[i]);

  }
  // Rcout << "Total:   " << constant_tape_point_.size() << "\n";
  // int sum=0; for(int i=0;i<constant_tape_point_.size();i++)sum+=constant_tape_point_[i];
  // Rcout << "Constant:" << sum << "\n";


  // Calculate pattern
  int m=Range();
  colpattern.resize(m);
  for(int i=0;i<m;i++)my_pattern(i);
  for(size_t i=0;i<op_mark_.size();i++)op_mark_[i]=0; /* remember to reset marks */
  for(size_t i=0;i<user_region_mark_.size();i++)user_region_mark_[i]=0; /* remember to reset marks */
}
