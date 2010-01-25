## does newdata come from the EMM?

setMethod("score", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata, method = c("prod", "sum", "log_odds"), 
		match_state="nn", plus_one = TRUE, 
		initial_transition = FALSE) 
	score(x, as.matrix(rbind(newdata)), method, 
		match_state, plus_one, initial_transition)
)

setMethod("score", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, method = c("prod", "sum", "log_odds"), 
		match_state="nn", plus_one = TRUE, 
		initial_transition = FALSE) 
	score(x, as.matrix(newdata), method, 
		match_state, plus_one, initial_transition)
)

setMethod("score", signature(x = "EMM", newdata = "matrix"),
        function(x, newdata, method = c("prod", "sum", "log_odds"), 
                match_state="nn", plus_one = TRUE, 
                initial_transition = FALSE) {

            method <- match.arg(method)

            if(method == "prod") {
                prob <- transition_table(x, newdata, method="prob", 
                        match_state, plus_one, 
                        initial_transition)[,3]
                return(prod(prob)^(1/length(prob)))
            }

            if(method == "sum") {
                prob <- transition_table(x, newdata, method="prob", 
                        match_state, plus_one, 
                        initial_transition)[,3]
                return(sum(prob)/length(prob))

            }

            if(method == "log_odds") {
                log_odds <- transition_table(x, newdata, method="log_odds", 
                        match_state, plus_one, 
                        initial_transition)[,3]
                return(sum(log_odds))
            }
        })
