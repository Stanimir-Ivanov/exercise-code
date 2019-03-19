function [ alpha ] = alpha_strongWolfe( func, grad, x, p, steplengthParam )
%ALPHA_STRONGWOLFE Line Search (Algorithm 3.5)
% func              - Function.
% grad              - Gradient.
% x                 - Current iterate.
% p                 - Search direction.
% steplengthParam   - Additional parameters for the algorithm, steplengthParam = [c1, c2, alpha1, alpha_max, gamma].

%assign additional params
c1 = steplengthParam(1);
c2 = steplengthParam(2);
alpha1 = steplengthParam(3);
alpha_max = steplengthParam(4);
gamma = steplengthParam(5);
%initialize variables
alpha_prev = 0;
alpha_current = alpha1;
first_iter = true;
%define phi and phi_prime
phi = @(alpha_formal) func(x + alpha_formal*p);
phi_prime = @(alpha_formal) grad(x + alpha_formal*p)'*p;

while(true)
	if (phi(alpha_current) > (phi(0) + c1*alpha_current*phi_prime(0)) || (phi(alpha_current) >= phi(alpha_prev) && ~first_iter ))
		alpha = alpha_strongWolfe_zoom(func, grad, x, p, alpha_prev, alpha_current, c1, c2);
		break;
	end
	if(abs(phi_prime(alpha_current)) <= -c2*phi_prime(0))
		alpha = alpha_current;
		break;
	end
	if(phi_prime(alpha_current) >= 0)
		alpha = alpha_strongWolfe_zoom(func, grad, x, p, alpha_current, alpha_prev, c1, c2);
		break;
	end
	alpha_prev = alpha_current;
	alpha_current = gamma*alpha_current;
	first_iter = false;
end
end