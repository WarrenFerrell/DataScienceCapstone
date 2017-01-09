env.print <- function(env) {
	for( vName in ls(env) ) {
	    cat(paste0(vName, ' : '))
	    v <- env[[vName]]
		if( is.environment(v) ) {
		    cat('\n\t')
			env.print(v)
		} else {
			cat(v)
		}
	}
}
