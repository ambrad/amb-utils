function [x y] = snbin(A,nmv,m,n)
% Stochastic matrix-free binormalization for nonsymmetric real A.
% [x y] = snbin(A,nmv,m,n)
%   A is a matrix or function handle. If it is a function handle, then
%     v = A(x) returns A*x and v = A(x,'trans') returns A'*x.
%   nmv is the number of forward and transpose matrix-vector product pairs to
%     perform.
%   m,n is the size of the matrix. It is necessary to specify these only if
%     A is a function handle.
%   diag(x) A diag(y) is approximately binormalized.
  
% Jan 2010. Algorithm and code by Andrew M. Bradley (ambrad@stanford.edu).
% Aug 2010. New omega schedule.
  
  op = isa(A,'function_handle');
  if(~op) [m n] = size(A); end
  r = ones(m,1); c = ones(n,1);
  for(k = 1:nmv)
    % omega^k
    alpha = (k - 1)/nmv;
    omega = (1 - alpha)*1/2 + alpha*1/nmv;
    % rows
    s = randn(n,1)./sqrt(c);
    if(op) y = A(s); else y = A*s; end
    r = (1-omega)*r/sum(r) + omega*y.^2/sum(y.^2);
    % columns
    s = randn(m,1)./sqrt(r);
    if(op) y = A(s,'trans'); else y = (s'*A)'; end
    c = (1-omega)*c/sum(c) + omega*y.^2/sum(y.^2);
  end
  x = 1./sqrt(r);
  y = 1./sqrt(c);
