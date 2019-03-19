function [ alpha ] = alpha_strongWolfe_zoom(func, grad, x, p, alphalo, alphahi, c1, c2)
%ALPHA_STRONGWOLFE_ZOOM Zoom (Algorithm 3.6)
% func              - Function.
% grad              - Gradient.
% x                 - Current iterate.
% p                 - Search direction.
% alphalo           - Left boundary of the zoom interval
% alphahi           - Right boundary of the zoom interval
% c1, c2            - Wolfe parameters satisfying 0 < c1 < c2 < 1

%define phi and phi_prime
phi = @(alpha_formal) func(x + alpha_formal*p);
phi_prime = @(alpha_formal) grad(x + alpha_formal*p)'*p;

while(true)
    alpha_current = (1/2)*(alphalo + alphahi);
    if(phi(alpha_current) > phi(0) + c1*alpha_current*phi_prime(0) || phi(alpha_current) >= phi(alphalo))
        alphahi = alpha_current;
    else
        if(abs(phi_prime(alpha_current)) <= -c2*phi_prime(0))
            alpha = alpha_current;
            break;
        end
        if(phi_prime(alpha_current)*(alphahi - alphalo) >= 0)
            alphahi = alphalo;
        end
        alphalo = alpha_current;
    end 
end
end