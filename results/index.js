const puppeteer = require('puppeteer');
const fs = require('fs');

const args = process.argv.slice(2);

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();


  if (!fs.existsSync(args[1])){
      fs.mkdirSync(args[1]);
  }

  await page.setViewport({
    width: 800,
    height: 800,
    deviceScaleFactor: 1,
  })


  await page.setContent(args[0])
  await page.addStyleTag({path: './results/style.css'})

  async function screenshotDOMElement(selector, path, padding = 0) {
    const rect = await page.evaluate(selector => {
      const element = document.querySelector(selector);
      const {x, y, width, height} = element.getBoundingClientRect();
      return {left: x, top: y, width, height, id: element.id};
    }, selector);

    return await page.screenshot({
      path: path,
      clip: {
        x: rect.left - padding,
        y: rect.top - padding,
        width: rect.width + padding * 2,
        height: rect.height + padding * 2
      }
    });
  }
  await page.screenshot({path: `${args[1]}${args[2]}.png`});
  // await screenshotDOMElement('body', `${args[1]}.png`)

  await browser.close();
})();

