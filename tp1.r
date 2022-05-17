sample_from <- function(p, n = 1) {

    get_x <- function() {
        p1 <- c(0, p[seq_len(length(p) - 1)])
        t <- runif(1)
        u <- (t > cumsum(p1)) & (t < cumsum(p))
        x <- which(u == 1)

        x
    }

    x <- rep(0, n)
    for (i in seq_len(n)) {
        x[i] <- get_x()
    }

    x
}

p <- c(1/2, 1/3, 1/6)

x <- sample_from(p, 1000)

hist(x)

etat_suiv <- function(x, Q) {
    p <- Q[x,]
    sample_from(p)
}


# ===================== Question 3 =================

Q <- matrix(c(1/3, 1/3, 1/3, 1/2, 0, 1/2, 0, 1/2, 1/2), byrow = TRUE, nrow = 3, ncol = 3)


sim_chain <- function(p0, Q, n = 50) {

    # get the first state
    x0 <- sample_from(p0)

    x <- rep(0, n)
    x[1] <- x0

    for (i in seq_len(n - 1)) {
        x[i + 1] <- etat_suiv(x[i], Q)
    }

    x
}

n_exp <- 20
t1 <- sim_chain(c(1, 0, 0), Q, n_exp)
t2 <- sim_chain(c(1, 0, 0), Q, n_exp)
t3 <- sim_chain(c(1, 0, 0), Q, n_exp)

df <- tibble(n = 1:n_exp, t1, t2, t3) |>
    pivot_longer(c(t1, t2, t3), names_to = "traj", values_to = "state")

df |> ggplot(aes(n, state, col = traj)) + geom_line() + facet_wrap(~traj)


eigen_out <- eigen(t(Q))
loi_stat <- eigen_out$vectors[,1]
loi_stat <- loi_stat / sum(loi_stat)

loi_stat

x <- sim_chain(c(1, 0, 0), Q, 1000)
loi_stat

# Calculate the portion spent in a certain state n
# given a simulation x
ft <- function(x, n) {
    cumsum(x == n) / seq_len(length(x))
}

ft1 <- ft(x, 1)
ft2 <- ft(x, 2)
ft3 <- ft(x, 3)

df_ft <- tibble(n = 1:1000, ft1, ft2, ft3) |>
    pivot_longer(c(ft1, ft2, ft3), names_to = "traj", values_to = "state")

df_ft |> ggplot(aes(n, state, col = traj)) + geom_line() + facet_wrap(~traj)


# ============== Question 4 ================#
n <- 50
Q <- matrix(c(0, 1, 0, 0.5, 0, 0.5, 0, 1, 0), nrow = 3, byrow = TRUE)
chain <- sim_chain(c(1, 0, 0), Q, n)
df <- tibble(n = 1:n, chain)
df |> ggplot(aes(n, chain)) + geom_line()



# ============== Question 5 ================#
n <- 50
Q <- matrix(c(0.5, 0.5, 0, 0, 0.5, 0.5, 0, 0, 0, 0, 2/3, 1/3, 0, 0, .25, .75), nrow = 5, byrow = TRUE)