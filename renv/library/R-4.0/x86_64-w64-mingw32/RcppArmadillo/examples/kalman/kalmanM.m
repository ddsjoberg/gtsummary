function Y = kalmanM(pos)
  dt=1;	
  %% Initialize state transition matrix
  A=[ 1 0 dt 0 0 0;...     % [x  ]
     0 1 0 dt 0 0;...     % [y  ]
     0 0 1 0 dt 0;...     % [Vx]
     0 0 0 1 0 dt;...     % [Vy]
     0 0 0 0 1 0 ;...     % [Ax]
     0 0 0 0 0 1 ];       % [Ay]
  H = [ 1 0 0 0 0 0; 0 1 0 0 0 0 ];    % Initialize measurement matrix
  Q = eye(6);
  R = 1000 * eye(2);
  x_est = zeros(6, 1);             % x_est=[x,y,Vx,Vy,Ax,Ay]'
  p_est = zeros(6, 6);

  numPts = size(pos,1);
  Y = zeros(numPts, 2);

  for idx = 1:numPts
    z = pos(idx, :)';
      
    %% Predicted state and covariance
    x_prd = A * x_est;
    p_prd = A * p_est * A' + Q;
    %% Estimation
    S = H * p_prd' * H' + R;
    B = H * p_prd';
    klm_gain = (S \ B)';
    %% Estimated state and covariance
    x_est = x_prd + klm_gain * (z - H * x_prd);
    p_est = p_prd - klm_gain * H * p_prd;
    %% Compute the estimated measurements
    Y(idx, :) = H * x_est;
  end                % of the function
   
end   % of the function
