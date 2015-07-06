n=5;                          //size of board
state = new Array(10);        // state of pieceij. 0=nobody, 1=player1, 2=player2
for(i=1;i<20;i++){
	state[i]= new Array(10);
}	
opponent=1;                   // 1:computer  0:player
_root.persOrComp.playWho.text = "PlAY COMPUTER";

////////// Initial conditions ////////////////////

function initialConditions(){
	freeze=0;                      // if freeze=1 then only reset works
	gotSquare=0;
	turn=1;                        //1=player1 2=player2
	go=0;                          //counts the number of goes to determine if draw
	for(i=1;i<10;i++) {
		for(j=1;j<10;j++) {
			state[i][j]=0;
		}
	}
	_root.conclusion._visible = false;
}


/////////////// Attach/detach pieces ////////////////////

function attachPieces(){
	for(i=1;i<n+1;i++) {
		for(j=1;j<n+1;j++) {
			duplicateMovieClip("piece", "piece"+i+j,11*i+j);
			_root["piece"+i+j]._x=i*400/(n+1);		
			_root["piece"+i+j]._y=j*400/(n+1);
			_root["piece"+i+j]._xscale=500/n;
			_root["piece"+i+j]._yscale=500/n;
		}
	}
}

function removePieces(){
	for(i=1;i<10;i++) {
		for(j=1;j<10;j++) {
			_root["piece"+i+j].removeMovieClip();
		}
	}
}




////////////// players choose a piece ////////////

function selectPiece(){
	for(i=1;i<10;i++) {
		for(j=1;j<10;j++) {
			_root["piece"+i+j].onPress = function() {
				I = +this._name.substr(5,1); 	
				J = +this._name.substr(6,1);
				if (state[I][J]==0 && freeze==0 && (turn==1 || opponent==0)){
					if (turn==1) {
						state[I][J]=1;
						col = new Color(_root["piece"+I+J]);
						col.setRGB(0xcc0000);
						squareTest(1);
					}
					if (turn==2) {
						state[I][J]=2;
						col = new Color(_root["piece"+I+J]);
						col.setRGB(0x009966);
						squareTest(2);
					}
					_root["piece"+I+J]._xscale=_root["piece"+I+J]._xscale * 1.5;
					_root["piece"+I+J]._yscale=_root["piece"+I+J]._yscale * 1.5;
					turn= (turn % 2)+1;
					go++; 
					if (go==n*n && freeze==0){
						gotSquare=1;
						_root.conclusion._visible = true;
						_root.conclusion.text = "Draw!";
					}
					if (opponent==1 && gotSquare==0){
						computerGo();
						squareTest(2);
						turn= (turn % 2)+1;
						go++; 
						if (go==n*n && freeze==0){
							_root.conclusion._visible = true;
							_root.conclusion.text = "Draw!";
						}
					}
				}
			}
		}
	}
}

/////////// test for square code///////////////

function squareTest(t){   //t=1 for player1, t=2 for player2
	j=1;
	while (gotSquare==0 && j<n+1){
		i=1;
		while (gotSquare==0 && i<n+1){
			if (state[i][j]==t){
				for (l=j;l<n+1;l++){
					if (l==j){start=i+1}else{start=1;}
					for (k=start;k<n+1;k++){
						if (state[k][l]==t){
							if ((0<k+l-j && k+l-j<n+1 && 0<l-k+i && l-k+i<n+1 &&
								 0<i+l-j && i+l-j<n+1 && 0<j-k+i && j-k+i<n+1 &&
								 state[k+l-j][l-k+i]==t && state[i+l-j][j-k+i]==t)||
						    	(0<k+j-l && k+j-l<n+1 && 0<l-i+k && l-i+k<n+1 &&
								 0<i+j-l && i+j-l<n+1 && 0<j-i+k && j-i+k<n+1 &&
								 state[k+j-l][l-i+k]==t && state[i+j-l][j-i+k]==t)){
								gotSquare=1; 
								_root.conclusion._visible=true;
								if (opponent==0 && turn==1){_root.conclusion.text="Player 1 wins!";}
								if (opponent==0 && turn==2){_root.conclusion.text="Player 2 wins!";}
								if (opponent==1 && turn==1){_root.conclusion.text="You win!";}
								if (opponent==1 && turn==2){_root.conclusion.text="Computer wins!";}
								freeze=1;
							}
						}
					}
				}
			}
			i++;
		}
		j++; 
	}
}

///////////// Increase/decrease n ///////////////

_root.nUp.onPress = function() {
	if (n<9 && go==0) {
		n++;
		removePieces();
		attachPieces();
		selectPiece();
		initialConditions();
	}
}

_root.nDown.onPress = function() {
	if (n>2 && go==0) {
		n--;
		removePieces();
		attachPieces();
		selectPiece();
		initialConditions();
	}
}

/////////////// reset code ////////////

_root.reset.onPress = function() {
	n=5;
	removePieces();
	attachPieces();
	selectPiece();
	initialConditions();
}


//////////// person or computer button ///////////

_root.persOrComp.onPress = function() {
	if (go==0) {
		opponent = (opponent+1)%2;
		if (opponent==1){
			_root.whoStart._visible=true;
			_root.persOrComp.playWho.text = "Play person";
		}else{
			_root.whoStart._visible=false;
			_root.persOrComp.playWho.text = "Play computer";
			
		}
	}
}

/////////// who starts button ////////////////////

_root.whoStart.onPress = function() {
	if (go ==0){
		randi= Math.floor(Math.random()*n)+1;
		randj= Math.floor(Math.random()*n)+1;
		state[randi][randj]=2;
		col = new Color(_root["piece"+randi+randj]);
		col.setRGB(0x009966);
		_root["piece"+randi+randj]._xscale=_root["piece"+randi+randj]._xscale * 1.5;
		_root["piece"+randi+randj]._yscale=_root["piece"+randi+randj]._yscale * 1.5;
		turn=1;
		go=1;
	}
}

////////// test for 3 squares ///////////////

function threeSquares(t){   //t=1 for player1, t=2 for player2
	j=1;
	while (j<n+1){
		i=1;
		while (i<n+1){
			if (state[i][j]==t){
				for (l=j;l<n+1;l++){
					if (l==j){start=i+1}else{start=1;}
					for (k=start;k<n+1;k++){
						if (state[k][l]==t){
							if  (0<k+l-j && k+l-j<n+1 && 0<l-k+i && l-k+i<n+1 && 
								 0<i+l-j && i+l-j<n+1 && 0<j-k+i && j-k+i<n+1){
								if (state[k+l-j][l-k+i]==t && state[i+l-j][j-k+i]==0) {alpha=i+l-j; beta=j-k+i; yes=1;}
								if (state[i+l-j][j-k+i]==t && state[k+l-j][l-k+i]==0) {alpha=k+l-j; beta=l-k+i; yes=1;}
							}
							if  (0<k+j-l && k+j-l<n+1 && 0<l-i+k && l-i+k<n+1 && 
								 0<i+j-l && i+j-l<n+1 && 0<j-i+k && j-i+k<n+1){
								if (state[k+j-l][l-i+k]==t && state[i+j-l][j-i+k]==0) {alpha=i+j-l; beta=j-i+k; yes=1;}
								if (state[i+j-l][j-i+k]==t && state[k+j-l][l-i+k]==0) {alpha=k+j-l; beta=l-i+k; yes=1;}
							}
						}
					}
				}
			}
			i++;
		}
		j++; 
	}
}

//////////// computer go ///////////////////

function computerGo() {
	tactic=1;                     //if can win, do so
	yes=0;
	threeSquares(2); 
	if (yes==0) { tactic=2;}

	if (tactic==2) {              //if player about to win, block  
		threeSquares(1);
		if (yes==0){ tactic=3;}
	}
	
	if (tactic==3) {
		for (j=1;j<n+1;j++) {
			for (i=1;i<n+1;i++) {	
				if (state[i][j]==2){
					for (l=j;l<n+1;l++){
						if (l==j){start=i+1}else{start=1;}
						for (k=start;k<n+1;k++){
							if (state[k][l]==2){
								if  (0<k+l-j && k+l-j<n+1 && 0<l-k+i && l-k+i<n+1 && 
									 0<i+l-j && i+l-j<n+1 && 0<j-k+i && j-k+i<n+1 &&
								 	state[k+l-j][l-k+i]==0 && state[i+l-j][j-k+i]==0){ 
	     							alpha=i+l-j; beta=j-k+i; yes=1;
								}
								if  (0<k+j-l && k+j-l<n+1 && 0<l-i+k && l-i+k<n+1 && 
									 0<i+j-l && i+j-l<n+1 && 0<j-i+k && j-i+k<n+1 &&
									state[k+j-l][l-i+k]==0 && state[i+j-l][j-i+k]==0) {
									alpha=i+j-l; beta=j-i+k; yes=1;
								}
							}
						}
					}
				}
			}
		}
	}
	if (yes==0){tactic=4;} 
	
	if (tactic==4){	       		  // go randomly
		do {
			alpha=Math.floor(Math.random()*n)+1;
			beta=Math.floor(Math.random()*n)+1;
		}while (state[alpha][beta] != 0)
	}
	
	state[alpha][beta]=2;
	col = new Color(_root["piece"+alpha+beta]);
	col.setRGB(0x009966);
	squareTest(2);
	_root["piece"+alpha+beta]._xscale=_root["piece"+alpha+beta]._xscale * 1.5;
	_root["piece"+alpha+beta]._yscale=_root["piece"+alpha+beta]._yscale * 1.5;
	// if two in a row, make a 3
	
	
}






initialConditions();
attachPieces();
selectPiece();
