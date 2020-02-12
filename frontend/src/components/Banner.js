import React, { useState } from 'react';

function Banner({children}) {
  const [open, setOpen] = useState(true);


  return (
    open
      ? (
        <div className='banner'>
          {children}
          </div>)
      : (<></>)
  );
}

export default Banner;
