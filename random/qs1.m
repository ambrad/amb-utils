function varargout = qs1(varargin)
% Some demo code for the Quake Seminar 9 Mar 2011.
%   The code and comments are intended to be a tutorial on a few related
% topics. Play around with some of the parameters. To run the code, type, for
% example,
%     qs1('uf_Test').
% at the Matlab command line.
%
% Contents:
%   1. How to assemble a sparse matrix: sma_Test
%   2. Some utility functions: TestIR
%   3. Factorizations and column or row updates: uf_Test
%   4. Linear least squares: WLS1, WLS2

% Andrew M. Bradley  ambrad@cs.stanford.edu
  [varargout{1:nargout}] = feval(varargin{:});
end

% ------------------------------------------------------------------------------
% 1. How to assemble a sparse matrix.
%   We assemble the 1D Laplacian in different ways. The matrix is
%     A = spdiags([-e 2*e -e],-1:1,n,n).
% This is the matrix that arises in an 1D linear elliptic problem (e.g., the
% heat equation) with homogeneous Dirichlet boundary conditions discretized by
% finite differences on a uniform mesh.

function sma_Test
  n = 100000;

  % Assemble A three different ways and time the results. Notice that
  % 'general' and 'special' are about equally fast and much faster than
  % 'bad'.
  tic; A1 = sma_Bad(n); fprintf(1,'bad et = %e\n',toc);
  tic; A2 = sma_General(n); fprintf(1,'general et = %e\n',toc);
  tic; A3 = sma_Special(n); fprintf(1,'special et = %e\n',toc);
  % chol is much faster than sma_Bad!
  tic; chol(A2); fprintf(1,'chol et = %e\n',toc);
end

function A = sma_Bad(n)
% Here is the wrong way to assemble a sparse matrix.

  % sparse(n,n) creates a completely empty sparse matrix.
  A = sparse(n,n);
  % Or try Jeremy's suggestion. It does not speed up the code in this case
  % because the work is still O(nnz^2), but for a very large problem where
  % memory traffic is an issue, it might. But in any case, this method in
  % general is bad:
  %A = sparse([],[],[],n,n,3*n);
  % The work to add a new element is linear in the current number of nonzeros
  % (nnz). Hence constructing a sparse matrix in this way takes
  %     O((final nnz)^2)
  % work.
  A(1,1) = 2;
  A(1,2) = -1;
  for (i = 2:n-1)
    A(i,i-1) = -1;
    A(i,i) = 2;
    A(i,i+1) = -1;
  end
  A(n,n-1) = -1;
  A(n,n) = 2;
end
  
function A = sma_General(n)
% This the right way to assemble a sparse matrix in the general case.
  
  % In the general case, we don't have a very good idea of the global structure
  % ahead of time, or perhaps we do, but it's very difficult to lay it out
  % statically. This approach lets us incrementally build the matrix. For
  % another description of this approach, see Tim Davis's blog post on the
  % topic here:
  %   http://blogs.mathworks.com/loren/2007/03/01/
  %     creating-sparse-finite-element-matrices-in-matlab/
    
  % I,J,S are vectors such that A(I(k),J(k)) = S(k). len keeps track of the
  % current amount of memory allocated to each vector. We use a
  % memory-doubling technique to avoid having to figure out how big the
  % I,J,S vectors need to be in advance.
  %   It would be a bad idea to set I = [] and then grow it one element at a
  % time (same for J and S). Matlab does not implement a memory-doubling-like
  % technique for an array, probably because that would result in a lot of
  % wasted allocated memory. So we have to do it instead. This technique is
  % quite useful in a number of applications.
  len = n; % Guess a reasonable starting size for the vectors
  I = zeros(len,1); J = I; S = I;
  
  I(1) = 1; J(1) = 1; S(1) =  2;
  I(2) = 1; J(2) = 2; S(2) = -1;
  k = 3;
  for (i = 2:n-1)
    I(k) = i; J(k) = i-1; S(k) = -1;
    k = k + 1;
    I(k) = i; J(k) = i;   S(k) =  2;
    k = k + 1;
    I(k) = i; J(k) = i+1; S(k) = -1;
    k = k + 1;
    if (k > len)
      % We need more space in the I,J,S vectors, so double their size.
      I = [I; zeros(len,1)];
      J = [J; zeros(len,1)];
      S = [S; zeros(len,1)];
      len = 2*len;
    end
  end
  I(k)   = n; J(k)   = n-1; S(k)   = -1;
  I(k+1) = n; J(k+1) = n;   S(k+1) =  2;

  % Now trim I,J,S down to the correct size.
  k = k + 1;
  I = I(1:k);  J = J(1:k); S = S(1:k);
  
  % Finally, hand off the vectors to 'sparse' to construct the sparse matrix.
  A = sparse(I,J,S,n,n);
end
  
function A = sma_Special(n)
% This is an example of the right way to assemble a sparse matrix when it has
% special structure.
  
  % Sometimes a sparse matrix has a simple enough structure that we can use a
  % combination of spdiags, kron, blkdiag, speye, sparse, and matrix
  % concatenation to assemble the sparse matrix efficiently. This is just about
  % the simplest example:
  e = ones(n,1);
  A = spdiags([-e 2*e -e],-1:1,n,n);
end
  
% ------------------------------------------------------------------------------
% 2. Some utility functions.

function f = Factorize(A)
% Factorize a full-rank square matrix.
  
  % Matlab has two data types for arrays: 'full' and 'sparse'.
  %   A full matrix A of size MxN stores M*N numbers in column-major order,
  % which means numbers are stored as [A(:,1); A(:,2); ...].
  %   A sparse matrix data structure encodes the locations of the nonzero
  % elements and the nonzero values (Matlab uses the CSC -- compressed sparse
  % column -- data structure, but this is usually apparent only in mex
  % files).
  %   We use the words 'dense' and 'sparse' to describe a qualitative
  % property of a matrix. A dense matrix has "a lot" of nonzeros, while a
  % sparse matrix has "not many".
  
  % Need A for some factorization update methods and for iterative
  % refinement.
  f.A = A;
  f.issparse = issparse(A); % Find out if A is of type 'full' or 'sparse'.
  if (f.issparse)
    % Matlab uses this factorization for sparse full-rank square \:
    %     A = R P' L U Q'.
    [f.L f.U f.P f.Q f.R] = lu(A);
  else
    % Matlab uses this factorization for dense full-rank square \:
    %     A = P' L U.
    % p is a permutation vector corresponding to P:
    %     P v = v(p).
    [f.L f.U f.p] = lu(A,'vector');
  end
end
  
function x = SolveAxb(f,b,use_ir)
% Solve A x = b using the factorization from Factorize.
%   If use_ir, use iterative refinement.
  if (nargin < 3) use_ir = false; end
  if (f.issparse)
    x = f.Q*(f.U \ (f.L \ (f.P*(f.R \ b))));
  else
    x = f.U \ (f.L \ b(f.p,:));
  end
  if (use_ir)
    % Iterative refinement solves an equation in which the right hand side is
    % the residual from the previous solve, then adds the result to the
    % solution:
    %     1. Solve A x = b
    %     2. Set r = b - A*x
    %     3. Solve A dx = r
    %     4. Set x = x + dx
    % This works because in step 4, mathematically (but not in finite
    % precision arithmetic),
    %     x = x + dx = x + inv(A) (b - A x) = x + inv(A) b - x = inv(A) b.
    x = x + SolveAxb(f, b - f.A*x, false);
  end
end

function re = RelErr(a,b)
% Relative error between a and b.
  re = norm(a - b,inf)/norm(a,inf);
end

function TestIR
  n = 505;
  A = sprandn(n,n,0.1,1e-11);
  A = full(A); % Comment out to leave as sparse
  b = randn(n,1);
  f = Factorize(A);
  
  % Matlab
  xt = A \ b;
  % LU
  x1 = SolveAxb(f,b);
  % LU with iterative refinement
  x2 = SolveAxb(f,b,true);
  fprintf('%e %e\n',RelErr(xt,x1),RelErr(xt,x2));
  % Results show that in the dense case, Matlab apparently does not use
  % iterative refinement. I think that differs from previous versions. Not
  % sure.
end

% ------------------------------------------------------------------------------
% 3. Factorizations and column or row updates.
%   Suppose one has a factorization of A, such as an LU factorization. Since
% it can be one of a number of different unsymmetric factorizations, let it
% be denoted by fac(A).
%   Now one wants to alter one or more columns in A. Is there an efficient
% way to do so while still retaining fac(A)? This section discusses a few
% different methods, pointing out where care must be taken in finite
% precision arithmetic. The same methods apply to updating a row.

function uf_Test
% Test some ideas on the dense problem
%     (A + (-A(:,i) + v) e_i') x = b,
% where one has fac(A).
  
  % Set up the problem.
  n = 505;
  A = sprandn(n,n,0.1,1e-6);
  b = randn(n,1);
  f = Factorize(A);

  % New column i. Make v very small to exhibit truncation error in poorly
  % implemented updates.
  i = 3;
  v = 1e-12*randn(n,1).^7;
  
  % Get the "true" solution.
  A1 = A; A1(:,i) = v;
  x_tr = A1 \ b;
  
  % Naive Sherman-Morrison-Woodbury method:
  x_smw = uf_SMW(f,i,v,b);
  % Correct Sherman-Morrison-Woodbury method:
  x_smw1 = uf_SMW1(f,i,v,b);
  % But, even better, the T-matrix method. This method is essentially
  % identical to the correct SMW method, but it also provides a very natural
  % way to calculate the condition number of the new factor implied by the
  % updated column.
  x_T = uf_T(f,i,v,b);
  
  % Relative errors:
  restrs = {'x_smw' 'x_smw1' 'x_T'};
  xs = [x_smw x_smw1 x_T];
  for (i = 1:length(restrs))
    fprintf(1,'re(x_true,%s) = %e\n',restrs{i},RelErr(x_tr,xs(:,i)));
  end
end
  
function x = uf_SMW(f,i,v,b)
% Replace column i of A with v and solve for x. Use the
% Sherman-Morrison-Woodbury formula with a rank-1 update.
  
  x = SolveAxb(f,b);
  % One problem with using the SMW formula is that
  %     d = v - A_i
  % can be inaccurate in finite precision. Suppose norm(v) << norm(A). Then d
  % suffers from truncation error: many of the digits of v are
  % lost. Cancellation error occurs when v ~= A_i, but it is benign.
  y = SolveAxb(f, -f.A(:,i) + v);
  x = x - x(i)/(1 + y(i))*y;
end
  
function x = uf_SMW1(f,i,v,b)
% Replace column i of A with v and solve for x. Use the
% Sherman-Morrison-Woodbury formula with a rank-1 update split into two
% updates.

  n = length(b);
  x = SolveAxb(f,b);
  % Let
  %     U = [-f.A(:,i) v],
  %     V = [e_i e_i].
  % Then solve
  %     (A + U V') x = b.
  % This formulation avoids truncation (and cancellation) error and is almost
  % identical to uf_T. The one problem is that it's not clear how to define
  % the condition number of the additional factor implied by the column
  % update.
  u = SolveAxb(f,v);
  C = zeros(2);
  C(2,1) = -1;
  C(:,2) = u(i);
  C(2,2) = C(2,2) + 1;
  c = C \ [x(i); x(i)];
  x(i) = x(i) + c(1);
  x = x - c(2)*u;
end
  
function x = uf_T(f,i,v,b)
% Replace column i of A with v and solve for x. Use the T-matrix method.
  
  % Form T. T can be defined as follows, but note that we need to keep only
  % the vector u:
  %     T = I + (u - e_i) e_i',
  % where
  %     A u = v.
  % Then
  %     A T = A (I + (u - e_i) e_i')
  %         = A + (A u) e_i' - (A e_i) e_i'
  %         = A + (v - A_i) e_i'.
  % Notice in the final line the the vector d = v - A_i appears; importantly,
  % it is not directly computed. Instead,
  %     A u = v
  % is solved, and then the difference
  %     delta = u - e_i
  % is computed. Both cancellation and truncation errors are avoided.
  %   Hence the factorization of the update matrix is
  %     fac(A) T
  % and we can use this factorization to solve our problem:
  %     (A + (-A_i + v) e_i') x = fac(A) T x = b;
  % so
  %     1. Solve fac(A) y = b.
  %     2. Solve T x = y.
  
  % Solve A u = v.
  u = SolveAxb(f,v);
  % Is T well-conditioned? This approach lets us measure the condition number
  % of the new factor very easily.
  %   inv(T) has the same structure as T. Let u_bar be to inv(T) as u is to T.
  u_bar = -u/u(i); u_bar(i) = u_bar(i) + 1 + 1/u(i);
  % The condition number of T in the 1-norm:
  c1n = max(1,norm(u,1)) * max(1,norm(u_bar,1));
  fprintf(1,'1-norm cond(T) = %e\n',c1n);
  % Solve A x = b.
  x = SolveAxb(f,b);
  % Solve T x = x.
  xp = x(i)/u(i); x = x - u*xp; x(i) = xp;
end

% ------------------------------------------------------------------------------
% 4. Linear least squares
  
function WLS1
% Walk through a combination of practical probability theory and linear
% algebra.
  
  % Writing x ~ N(mu, W) means that x is a normal random vector (rv) with mean
  % mu and covariance W. A property of a normal rv x is that an affine
  % transformation of it is also normal; in particular,
  %     v + B x ~ N(v + B mu, B W B').   (p1)
    
  % Let A have full rank in the model    
  %   A x = b + N(0, W).
  m = 100;
  n = 70;
  rankA = n;
  [W A b] = RandomWLS(m,n,rankA);  
  % Weighting comes from "whitening" the noise: that is, transforming the
  % measurements so that they are iid. Let
  %     W = Wf Wf'.
  % I use 'inv' here only in an analytical sense: don't actually use it in
  % practice. By (p1),
  %     inv(Wf) A x_hat = inv(Wf) b + N(0, I).
  % The esimtator x_hat is
  %     x_hat = arg min_{x_hat} || inv(WF) A x_hat - inv(Wf) b ||_2
  %           = inv(A' inv(W) A) A' inv(W) b.
  % What is the covariance matrix of the estimator, cov(x_hat)? Applying (p1),
  %     x_hat = inv(A' inv(W) A) A' inv(W) b + N(0, inv(A' inv(W) A)).
  % Hence cov(x) = inv(A' inv(W) A). We compute this as follows:
  %     1. Cf = (Wf \ A) \ eye(m)
  %     2. cov(x_hat) = Cf Cf'.
  % This works because after step 1,
  %     Cf = inv(A' inv(W) A) A' inv(Wf'),
  % and so
  %     Cf Cf' = inv(A' inv(W) A) A' inv(Wf') inv(Wf) A inv(A' inv(W) A)
  %            = inv(A' inv(W) A) (A' inv(W) A) inv(A' inv(W) A)
  %            = inv(A' inv(W) A).
  % Moreover, we can get both x_hat and cov(x_hat) using one backslash, which
  % means that just one factorization is used to solve multiple problems.
  Wf = chol(W,'lower');
  WfA = Wf \ A;
  Wfb = Wf \ b;
  C = WfA \ [Wfb eye(m)];
  x_hat = C(:,1);
  C = C(:,2:end);
  C = C*C'; % cov(x_hat)

  if (m < 1000) % check
    % This is an inefficient and inaccurate way to get x_hat:
    xh1 = (A'*(W\A)) \ (A'*(W\b));
    RelErr(xh1,x_hat)
    % Same for cov(x_hat):
    RelErr(C,inv(A'*inv(W)*A))
  end
  
  % For full matrices A, the method we just discussed is not a bad way to
  % obtain C. However, C is usually dense even if A is sparse. Hence if A is
  % sparse, we need to use a different method to obtain cov(x_hat), or else
  % our memory will blow up when we explicitly form it. A good method is
  % demonstrated here.
  m = 900;
  n = 700;
  rankA = n;
  [W A b] = RandomSparseWLS(m,n,rankA);
  % Obtain the Cholesky factor of sparse W. S is a column permutation that
  % helps to minimize the fill-in, the number of nonzeros (nnz) in the
  % factor Wf in excess of those in W.
  [Wf p S] = chol(W,'lower','matrix');
  WfA = Wf \ (S'*A);
  Wfb = Wf \ (S'*b);
  % We can of course compute
  %     x_hat = WfA \ WfB
  % as before. However, the work to factorize WfA will be wasted. Instead, we
  % compute the QR factorization and can then use R later for
  % cov(x_hat). The factorization gives us orthonormal Q, rectangular upper
  % triangular R, and a permutation matrix P such that WfA = Q R P'. As
  %     || WfA x_hat - Wfb ||_2 = || (Q R P') x_hat - Wfb ||_2
  %                             = || Q' (Q R P') x_hat - Q' Wfb ||_2
  %                             = || R (P' x_hat) - Q' Wfb ||_2,
  % (because || Q v ||_2 = || v ||_2 for an orthonormal matrix Q), we need to
  % solve
  %     R(1:n,1:n) (P' x_hat) = Q(:,1:n)' Wfb.
  %   Though WfA is sparse, Q is almost certainly dense, and so we cannot
  % afford to have it formed. The sparse Q-less QR factorization of
  % Q allows us to pass Wfb so that on output,
  %     QtWfb = Q' Wfb.
  % The factor Q is never explicitly formed; rather, as the factorization
  % proceeds, each additional elementary orthogonal transformation is applied to
  % b.
  %   p is a permutation vector that represents the permutation matrix P'. As
  % with chol, we want qr to have the freedom to permute WfA as necessary to
  % minimize fill-in in R.
  %   Passing 0 as the third argument requests 'economy size' results. R is
  % just the square upper triangular part. If Q were requested, it would have
  % the same size as WfA. QtWfb is Q(:,1:n)' Wfb rather than Q' Wfb.
  [QtWfb R p] = qr(WfA,Wfb,0);
  x_hat = R \ QtWfb;  % => P' x_hat
  x_hat(p) = x_hat;   % => x_hat
  % What about cov(x_hat)? In fact, we're done, for
  %     R = chol(inv(cov(x_hat))):
  % that is, R is the Cholesky factor of the inverse of a symmetric
  % permutation of the covariance of the estimator:
  %     WfA = Q R P' and so
  %     cov(x_hat) = inv(WfA' WfA) = inv(P R' Q' Q R P') = inv(P R' R P')
  %                = P inv(R' R) P'
  %     => inv(P' cov(x_hat) P) = R' R.
  % To get, say, the variance of the estimator x_hat(i), do the following:
  i = n - 10;
  ei = sparse(i,1,1,m,1);   % I(:,5) for I = eye(m)
  vi(p) = R \ (R' \ ei(p)); % Solve (P inv(R' R) P') vi = ei
  vi = vi(i);               % vi(i) is C(i,i), where C = cov(x_hat).

  if (m < 1000) % check
    xh1 = (A'*(W\A)) \ (A'*(W\b));
    RelErr(xh1,x_hat)
    C = inv(A'*(W\A));
    RelErr(vi,C(i,i))
  end
end
  
function WLS2
% Demonstrate various ways to solve the dense linear least squares problem
%     A x = b + N(0, W),
% where A is rank-deficient.
  
  % Set up the random problem
  %     A x = b + N(0, W).
  m = 50;
  n = 35;
  rankA = n - 8;
  % Rather than create an exactly rank-deficient matrix A -- which would make
  % things too easy -- add some noise. Try changing this to, say, 1e-11.
  noise = 1e-14;
  [W A b] = RandomWLS(m,n,rankA,noise);

  % Factorize W and apply to A and b.
  Wf = chol(W,'lower');
  WfA = Wf \ A;
  Wfb = Wf \ b;
  
  % 1. Use the SVD to find the pseudoinverse. If A is rank deficient, this
  % method finds the minimum-||x||_2 solution.
  x_svd = pinv(WfA)*Wfb;
  % In all print statements, resn = residual norm; re = relative
  % error. re(method) means that the error is being computed relative to the
  % solution produced by the method 'method'.
  fprintf(1,'pinv: resn = %e  ||x||_2 = %e\n',...
	  norm(WfA*x_svd - Wfb),norm(x_svd));

  % 2. We used 'pinv' in 1. For reference, let's solve the problem using our own
  % SVD call. Quite often we want to plot the singular values of WfA.
  [U S V] = svd(WfA);
  s = diag(S);
  % Same tolerance as pinv's default. This is actually the wrong tolerance to
  % use if 'noise' above is >= ~1e-12. If we were solving this problem in
  % practice, we would plot the singular values ('semilogy(s)') and decide on
  % the cutoff.
  tol = max(size(WfA))*s(1)*eps(class(WfA));
  K = find(s < tol,1);
  if (isempty(K)) K = n + 1; end
  K = K - 1;
  fprintf(1,'K = %d  n = %d\n',K,n);
  % Solve the problem given the estimated numerical rank K.
  x_svd1 = V(:,1:K)*(diag(1./s(1:K))*(U(:,1:K)'*Wfb));
  fprintf(1,'svd: re(pinv) = %e  resn = %e\n',...
	  RelErr(x_svd,x_svd1),norm(WfA*x_svd1 - Wfb));
  
  % 3. Use backslash, whose implementation is explained in 4. Expect to see a
  % warning about the rank of the problem on the Matlab command line.
  x_bs = WfA \ Wfb;
  fprintf(1,'\\: re(svd) = %e  resn = %e  ||x||_2 = %e\n',...
	  RelErr(x_svd,x_bs),norm(WfA*x_bs - Wfb),norm(x_bs));
  
  % 4. QR with pivoting, which is what backslash does. On output,
  %     WfA = Q R P'.
  WfA = WfA;
  [Q R P] = qr(WfA);
  % Determine the rank of WfA. Choose a tolerance tol such that any diagonal
  % entry of R below tol is considered to be 0.
  tol = max(size(WfA))*norm(WfA,inf)*eps;
  % This lets us determine the numerical rank K of R. As R is ordered so that
  % abs(diag(R)) is in decreasing order, we just have to find the first such
  % element; all subsequent ones also are below tol.
  K = find(abs(diag(R)) < tol,1);
  % (If all diagonal elements are above tol, then the rank is just n.)
  if (isempty(K)) K = n + 1; end
  K = K - 1; % Numerical rank of R
  fprintf(1,'K = %d  n = %d\n',K,n);
  % Let y = P' x. As WfA has rank K, R is a trapezoid (a triangle with its
  % top cut off), and so the problem is reduced to one that is KxK. We must
  % solve the linear equation
  %     R(1:K,1:K) y(1:K) = Q(:,1:K)' Wfb
  % for y and set
  %     x = P y.
  % But this does not find the minimum-norm x; rather, it finds x such that
  % only K components are nonzero.
  y = R(1:K,1:K) \ (Q(:,1:K)'*Wfb);
  x_qr = P*[y; zeros(n-K,1)];
  % Method 4 may differ from method 3 since our tolerance for rank detection
  % is almost surely different than \'s.
  fprintf(1,'qr: re(svd) = %e  re(\\) = %e  resn = %e\n',...
	  RelErr(x_svd,x_qr),RelErr(x_bs,x_qr),norm(WfA*x_qr - Wfb));
end  
  
function [W A b] = RandomWLS(m,n,rankA,noise)
% Create a random dense least squares problem.

  if (nargin < 4) noise = 0; end
  
  % Covariance matrix W
  k = 0; % Let's not deal with rank-deficient W
  Wpd = k == 0;
  [W,~] = qr(randn(m,m-k),0);
  s = randn(m-k,1).^2;
  W = W*diag(s)*W';
  
  % Matrix A
  k = n - rankA;
  [A,~] = qr(randn(m,n-k),0);
  V = qr(randn(n,n-k),0);
  s = randn(n-k,1).^2;
  A = A*diag(s)*V';
  % Life would be too easy if A were so obviously rank deficient. Add a small
  % factor times I to make numerical rank determination harder.
  A = A + noise*max(A(:))*eye(m,n);
  
  % b
  x = randn(n,1);
  R = chol(1e-6*eye(m) + W);
  b = A*x + R'*randn(m,1);
end
  
function [W A b] = RandomSparseWLS(m,n,rankA,noise)
% Create a random sparse least squares problem.

  if (nargin < 4) noise = 0; end
  
  % Covariance matrix W
  W = sprandn(m,3,0.1);
  W = spdiags(W,-1:1,m,m);
  W = W + W' + 100*speye(m);
  
  % Matrix A
  A = sprandn(m,rankA,10/m,1e-3);
  for (i = rankA+1:n)
    p = randperm(rankA);
    p = p(1:2);
    v = zeros(rankA,1);
    v(p) = randn(2,1);
    A = [A(:,1:rankA)*v A];
  end
  A = A + noise*norm(A,inf)*speye(m,n);
  
  % b
  x = randn(n,1);
  R = chol(1e-6*eye(m) + W);
  b = A*x + R'*randn(m,1);
end
