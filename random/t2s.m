function varargout = t2s(varargin)
% A tutorial on stability and accuracy. Solutions.
%   This tutorial studies the concepts of "linear stability" and "order of
% accuracy" of ODE time integration methods. It is meant to be read and
% interacted with. Look for the word "Exercise" to indicate missing parts
% that you should fill in.  
%   The accompanying file t2s.p lets you run the completed code so you can
% see how it should behave.
%   The file t2s.m, distributed later, has solutions to all the exercises.
%   As an example, at the Matlab command line, type
%     k_max = t2s('ee_MaxTimeStep', -1 + 2i)
% to find out the maximum stable time step for the explicit Euler problem on
% the function dy/dt = (-1 + 2i) y. When you complete Exercise 4, then
%     k_max = t2('ee_MaxTimeStep', -1 + 2i)
% will give the same value for k_max.
%
% AMB ambrad@cs.stanford.edu Feb 2013
  [varargout{1:nargout}] = feval(varargin{:});
end

function RunAll()
% Run each piece of this tutorial. These won't work until you've finished the
% associated exercises. Copy a line to the command line to run just the part
% you are working on.
  figure(1); clf;
  t2s('ee_StudyStabilityForFixedLambda', complex(-.1,1), 30);

  figure(2); clf;
  t2s('StudyStability', @(varargin)t2s('ee_Integrate', varargin{:}));

  figure(3); clf;
  t2s('StudyStability', @(varargin)t2s('ie_Integrate', varargin{:}));

  figure(4); clf;
  t2s('StudyStability', @(varargin)t2s('mpr_Integrate', varargin{:}));
  
  figure(5); clf;
  lambda = -0.5 + 3i;
  tf = 10;
  nrefine = 10;
  k_max = t2s('ee_MaxTimeStep', lambda);
  args = {lambda, tf, k_max, nrefine};
  t2s('StudyAccuracy', @(varargin)t2s('ee_Integrate', varargin{:}), args{:});
  hold all;
  t2s('StudyAccuracy', @(varargin)t2s('ie_Integrate', varargin{:}), args{:});
  t2s('StudyAccuracy', @(varargin)t2s('mpr_Integrate', varargin{:}), args{:});
end

% ------------------------------------------------------------------------------
% ODE methods.

function t = DiscretizeTime(tspan, k)
% Given the time span tspan(1) to tspan(2) and the time step k, create the
% array of time points t.
  ny = ceil((tspan(2) - tspan(1))/k) + 1;
  t = tspan(1):k:tspan(2);
  if (numel(t) < ny) t(end+1) = tspan(2); end
end

function [t y] = ee_Integrate(A, y0, tspan, k)
% Integrate dy/dt = A*y, y(tspan(1)) = y0, from tspan(1) to tspan(2) using steps
% of length k. The last step may be smaller than k. Use the explicit Euler
% method.
  t = DiscretizeTime(tspan, k);
  nt = numel(t);
  y = zeros(numel(y0), nt);
  
  y(:,1) = y0(:);
  for (i = 2:nt)
    k = t(i) - t(i-1);
    % The explicit Euler time step is derived from the relation
    %     (y[n+1] - y[n]) / k = A y[n].
    y(:,i) = y(:,i-1) + k*A*y(:,i-1);
  end
end

function [t y] = mpr_Integrate(A, y0, tspan, k)
% Integrate dy/dt = A*y, y(tspan(1)) = y0, from tspan(1) to tspan(2) using steps
% of length k. The last step may be smaller than k. Use the midpoint rule.
  t = DiscretizeTime(tspan, k);
  nt = numel(t);
  y = zeros(numel(y0), nt);
  
  y(:,1) = y0(:);
  for (i = 2:nt)
    k = t(i) - t(i-1);
    % The midpoint rule time step is derived from the relation
    %     (y* - y[n]) / (k/2) = A y[n]
    %     (y[n+1] - y[n]) / k = A y*.
    % Exercise 1. Write the code for the implicit Euler step.
    y(:,i) = y(:,i-1) + k*A*(y(:,i-1) + k/2*A*y(:,i-1));
  end
end

function [t y] = ie_Integrate(A, y0, tspan, k)
% Integrate dy/dt = A*y, y(tspan(1)) = y0, from tspan(1) to tspan(2) using steps
% of length k. The last step may be smaller than k. Use the implicit Euler
% method.
  t = DiscretizeTime(tspan, k);
  nt = numel(t);
  y = zeros(numel(y0), nt);
  
  y(:,1) = y0(:);
  for (i = 2:nt)
    k = t(i) - t(i-1);
    % The implict Euler time step is derived from the relation
    %     (y[n+1] - y[n]) / k = A y[n+1].
    % Exercise 2. Write the code for the implicit Euler step.
    y(:,i) = (eye(size(A)) - k*A) \ y(:,i-1);
  end
end

function test_Integrators()
% Exercise 3. Test your implementation of the ODE integrators.
end

% ------------------------------------------------------------------------------
% Stability analysis.

function k = ee_MaxTimeStep(lambda)
% Obtain the maximum stable step size for the explicit Euler method on the
% test function.
  
  %   Consider the test function y given by
  %     { dy/dt = lambda y
  %     { y(t = 0) = y0.
  % The solution is
  %     y(t) = y0 exp(lambda t).
  % This function is called the "test function" because it is used to perform
  % linear stability analysis of ODE integrators. One can think of the scalar
  % test function as one component of a system of linear ODE that has been
  % diagonalized. Hence the test function is a very useful analysis tool. The
  % test function has this property:
  %     P1. If real(lambda) <= 0, |y(t)| <= |y(0)| for t >= 0.
  %   We want the explicit Euler method to have the same property:
  %     P2. If real(lambda) <= 0, |y[n]| <= |y[1]| for n >= 1.
  % (Notice we use () for the argument of a function and [] to index an
  % array.) If an integrator has the property P2, then it is stable. An
  % unstable integrator "blows up", or grows without bound, even though the
  % ODE being integrated does not.
  %
  %   The explicit Euler method can be written
  %     y[n+1] = (1 + k lambda) y[n]
  %            = (1 + k lambda)^n y[1].
  % Let z = k lambda.
  %     F3. If |1 + z| <= 1, then |y[n])| <= |y[1]| for n >= 1.
  % Write
  %     |1 + z| = r exp(i theta) for r, theta real.
  % Then
  %     r^2 = |1 + z|^2.
  % Our key tool is this fact:
  %     F4. |(1 + z)^n| <= 1 for n >= 1  iff  r^2 <= 1.
  %   Proof. |(1 + z)^n| = |r^n exp(i theta n)| = r^n. r^2 <= 1 iff r <= 1 iff
  % r^n <= 1.
  %
  %   Let:
  a = real(lambda);
  if (a > 0) error('Required: real(lambda) <= 0.'); end
  b = imag(lambda);
  % Assume a <= 0 so that P1 applies. Write
  %     |1 + k a + i k b| = r exp(i theta).
  % We want r^2 <= 1.
  %
  % Exercise 4. Derive the expression for the maximum stable time step k.
  %   We have
  %     r^2 = |1 + z|^2
  %         = (1 + k a)^2 + (k b)^2
  %         = (a^2 + b^2) k^2 + 2 a k + 1.
  % and so
  %     (a^2 + b^2) k^2 + 2 a k <= 0
  %     k <= -2 a / (a^2 + b^2).
  k = -2*a / (a^2 + b^2);
end

function k = ie_MaxTimeStep(lambda)
% Maximum time step size for the implicit Euler method.
  
  % Exercise 6. Derive the expression for the maximum stable time step k. (If
  % the expression gives k < 0, then we can set k = inf.)
  %   The implicit Euler method can be written (recall z = k lambda):
  %     y[n+1] = y[n] / (1 - z) = y[1] / (1 - z)^n.
  %     F5. If |1 / (1 - z)| <= 1, then |y[n]| <= |y[1]| for n >= 1.
  %   Write
  %     |1 / (1 - z)| = r exp(i theta) for r, theta real.
  % Then
  %     r^2 = 1 / |1 - z|^2.
  %     F6. |1 / (1 - z)^n| <= 1 for n >= 0  iff  r^2 <= 1.
  %   Let:
  a = real(lambda);
  b = imag(lambda);
  % Write
  %     1 / (1 - k a - i k b) = r exp(i theta).
  % We want r^2 <= 1. We have
  %     r^2 = 1 / ((1 - k a)^2 + (k b)^2) <= 1
  % and so we want
  %     k >= 2*a / (a^2 + b^2).
  % If a <= 0, then we can set k = inf: any time step gives a stable method.
  if (a <= 0)
    k = inf;
  else
    % Notice k >= 2*a / (a^2 + b^2) implies this is in fact not a *maximum*
    % time step, but rather a minimum time step. The idea is that if
    % real(lambda) > 0, then k has to be sufficiently large to damp the
    % numerical solution.
    k = 2*a / (a^2 + b^2);
  end
end

function ee_StudyStabilityForFixedLambda(lambda, tf)
% Given lambda and the final time tf, plot abs(y(tf)) for various values of
% the time step k.

  % Analytical solution.
  t = linspace(0, tf, 1000);
  y_true = exp(lambda*t);
  
  % Try various factors of k_max.
  k_max = ee_MaxTimeStep(lambda);
  kfs = [0.5 0.99 1 1.01 2];
  
  yfn = @(y) log10(abs(y));
  hs(1) = plot(t, yfn(y_true), 'k-'); set(hs, 'linewidth', 2); hold all;
  xlabel('t'); ylabel('log_{10} abs(y(t))'); drawnow;
  lgd = {'true'};  
  for (i = 1:numel(kfs))
    [t y] = ee_Integrate(lambda, 1, [0 tf], kfs(i)*k_max);
    hs(end+1) = plot(t, yfn(y), '-'); drawnow;
    lgd{end+1} = sprintf('k = %1.2f k_{max}', kfs(i));
  end
  legend(hs, lgd, 'location', 'best');
  title(sprintf('lambda = %1.1f + i %1.1f, k_{max} = %1.2f',...
                real(lambda), imag(lambda), k_max));
end

function StudyStability(integrator)
% Another way to view stability is to ask in what region of the complex plane
% z = k lambda can lie so that the integrator is stable.
%   For the explicit Euler method, by F3, the region is
%     |1 + z| <= 1.
% This is everything inside a circle having radius 1 centered at -1 + 0i.
%
% Exercise 7. What is the region for the implicit Euler method?
%   The region is given by
%     1 / |1 - z| <= 1.
% This is everything outside of the unit circle centered at 1 + 0i.
%   The implicit Euler method "damps" the ODE. Even if the analytical solution
% grows without bound (because real(lambda > 0)), the implicit-Euler numerical
% solution does not if z is in the stable region. This type of behavior suggests
% stability does not imply accuracy. (Indeed, a simple stable method is one
% that sets y[n] = y0 for n >= 0. Of course, this method is almost always
% inaccurate!)
%
% This function makes a plot that shows the numerically obtained stable region
% in the complex plane for the given integrator. 'integrator' is a function
% handle. For example, it could be this:
%     integrator = @(varargin)t2s('ee_Integrate', varargin{:}).
% One approach is to generate random z = k lambda, integrate to a fixed final
% time tf, and determine whether numerically |y(tf)| <= |y(0)|.
%
% Exercise 5. Implement this routine however you wish.
    
  % Fix k = 1 and generate random lambda.
  n_lambda = 4000;
  tf = 10;
  randfn = @(varargin) 2*(rand(varargin{:}) - 0.5);
  lambdas = 3*randfn(n_lambda,1) + 3*1i*randfn(n_lambda,1);
  % For each lambda, integrate to tf and record the final magnitude.
  final_abs = zeros(n_lambda,1);
  for (i = 1:n_lambda)
    % dy/dt = f(y).
    [t y] = integrator(lambdas(i), 1, [0 tf], 1);
    final_abs(i) = abs(y(end));
  end

  m = final_abs > 1;
  plot(real(lambdas(~m)), imag(lambdas(~m)), 'b.',...
       real(lambdas(m)), imag(lambdas(m)), 'r.');
  % Circle of instability for implicit Euler.
  h = DrawCircle([1 0], 1, 100); set(h, 'color', 'g');
  % Circle of stability for explicit Euler.
  h = DrawCircle([-1 0], 1, 100); set(h, 'color', 'g');
  legend('Stable z = k \lambda', 'Unstable z');
  xlabel('real z'); ylabel('imag z');
  title(SlashUnderscore(sprintf('Integrator: %s', func2str(integrator))));
  axis equal; axis tight;
end

% ------------------------------------------------------------------------------
% Order of accuracy analysis.

function StudyAccuracy(integrator, lambda, tf, k_max, nrefine)
% Obtain the empirical order of accuracy of 'integrator' for the test
% function. tf is the time to which to integrate. k_max is the maximum time
% step considered. Refine by halving the time step nrefine times.
  
  % Order of accuracy is obtained most straightforwardly by Taylor series
  % expansion.
  %
  % Consider the ODE
  %     dy/dt = f(t, y).
  % Assume y[n] = y(k n). Let t = k n. Let us derive the error of the explicit
  % Euler method at time k (n+1) = t+k:
  %     y[n+1] - y(t+k)
  %       = [y[n] + k f(t, y[n])] - [y(t) + k f(t, y(t)) + O(k^2)]
  %       = O(k^2).
  % At first glance, it might appear that the order of accuracy is 2 because of
  % the '2' in the exponent. But we have to decrement the exponent by one to get
  % the order because we want to consider the error over a fixed time interval
  % T. If we take a step of size k, then we must take ~T/k steps. Then the
  % accumulated error is
  %     sum_{T/k steps} O(k^2) = T/k O(k^2) = T O(k),
  % which shows the method has first-order accuracy.
  %
  % The key technique of the analysis of the order of accuracy is substituting
  % the exact solution into the scheme and then using Taylor series expansions
  % to cancel terms until only a term O(k^{p+1}) remains for a p'th-order
  % method.
  %
  % Exercise 8. Determine the order of accuracy of the implicit Euler method.
  %   This is determined by obtaining the order of the following difference:
  %     [y(t) + k f(t+k, y(t+k))] - y(t+k),
  % which can be written as
  %     [y(t) + k y'(t+k)] - y(t+k),
  % where y' = dy/dt. We will need
  %     y'(t+k) = y'(t) + k y''(t) + O(k^2).
  % We have
  %     [y(t) + k y'(t+k)] - y(t+k)
  %       = [y(t) + k (y'(t) + k y''(t) + O(k^2))] -
  %         [y(t) + k y'(t) + O(k^2)]
  %       = O(k^2).
  %   The method has order of accuracy 2-1 = 1.
  %
  % Exercise 9. Determine the order of accuracy of the midpoint rule for the
  % problem dy/dt = f(y). We will need
  %     f(y(t) + k/2 y'(t)) = f(y(t)) + f(y(t))_y k/2 y'(t) + O(k^2)
  % Recognize
  %     f(y(t)) = y'(t)
  % and
  %     f(y(t))_y y'(t) = y''(t),
  % and so
  %     f(y(t) + k/2 y'(t)) = y'(t) + k/2 y''(t) + O(k^2).
  % Hence
  %     [y(t) + k f(y(t) + k/2 f(t, y(t)))] - y(t+k)
  %       = [y(t) + k f(y(t) + k/2 y'(t))] - y(t+k)
  %       = [y(t) + k y'(t) + k^2/2 y''(t) + O(k^3)] -
  %         [y(t) + k y'(t) + k^2/2 y''(t) + O(k^3)]
  %       = O(k^3),
  % and so the method has order of accuracy 3-1 = 2.
  %
  % Now let's look at how to do an empirical study of the order of accuracy of a
  % method. This procedure, or something like it, should be used to validate
  % implementations of numerical integrators. When integrating PDE, often a bug
  % in the implementation of the boundary conditions can degrade the order of
  % accuracy relative to what is theoretically expected, and a validation of
  % this sort can reveal the presence of such a bug.
  
  % We need an analytical solution against which we can compare. In a more
  % sophisticated setting involving linear or nonlinear PDE with boundary
  % conditions, we might have to introduce a source term. Google "method of
  % manufactured solution" for much more. But here we can use the test
  % function.
  fn_yt = @(t) exp(lambda*t);
  % A simple norm for our problem. Since we are dealing with the
  % discretization of a continuous field, the time step must enter the norm.
  L2norm = @(t, y) norm(y(2:end).*diff(t), 2);

  % re stores the norm-wise relative error in the solution for each
  % refinement level.
  re = zeros(1, nrefine+1);
  for (i = 0:nrefine)
    [t y] = integrator(lambda, 1, [0 tf], k_max / 2^i);
    yt = fn_yt(t);
    % Norm-wise relative error:
    re(i+1) = L2norm(t, y - yt) / L2norm(t, yt);
  end
  
  % A log-log plot reveals the order of accuracy.
  loglog(2.^(0:nrefine), re, '.-');
  xlabel(['(# time steps with k = k_{max} / 2^i) / ',...
          '(# time steps with k = k_{max})']);
  ylabel('Norm-wise relative error');
  % Exercise 10. Calculate the empirically obtained order of accuracy based on
  % the final two integrations.
  ooa = -log(re(end)/re(end-1)) / log(2);
  fprintf('Empirically obtained order of accuracy: %1.2f\n', ooa);
end

% ------------------------------------------------------------------------------
% Util.

function h = DrawCircle(ctr, r, n)
% Draw a circle centered at ctr with radius r using n lines.
  theta = linspace(0, 2*pi, n+1);
  x = ctr(1) + r*cos(theta);
  y = ctr(2) + r*sin(theta);
  h = line(x, y);
end

function s = SlashUnderscore(s)
% Place a backslahsh before underscores for Matlab plotting.  
  function s = SU(s)
    k = findstr(s,'_');
    for(i = 1:length(k))
      j = k(i)+i-1;
      s = [s(1:j-1) '\' s(j:end)];
    end
  end
  if (iscell(s))
    for(i=1:length(s)) s{i} = SU(s{i}); end
  else
    s = SU(s);
  end
end
