// ============================================================
// jmig_2017_scraper.js
//
// Run this in Chrome DevTools console while on any jmig.org page.
// It fetches all 96 AAGL 2017 abstract pages using your browser's
// authenticated session (bypasses Cloudflare) and downloads the
// results as jmig_2017_abstracts.json.
//
// HOW TO USE:
//   1. Open Chrome and navigate to any jmig.org page (e.g., one of the links above)
//   2. Open DevTools: Cmd+Option+J
//   3. Paste this entire script into the Console and press Enter
//   4. Wait ~2 minutes for all 96 pages to load
//   5. A file "jmig_2017_abstracts.json" will download automatically
//   6. Move it to: abstract_lifetime/data/cache/jmig_2017_abstracts.json
//   7. Run: Rscript scripts/ingest_jmig_2017_json.R
// ============================================================

const piis = [
  'S1553-4650(17)30453-3','S1553-4650(17)30454-5','S1553-4650(17)30455-7',
  'S1553-4650(17)30456-9','S1553-4650(17)30457-0','S1553-4650(17)30458-2',
  'S1553-4650(17)30459-4','S1553-4650(17)30460-0','S1553-4650(17)30461-2',
  'S1553-4650(17)30462-4','S1553-4650(17)30463-6','S1553-4650(17)30464-8',
  'S1553-4650(17)30465-X','S1553-4650(17)30466-1','S1553-4650(17)30467-3',
  'S1553-4650(17)30468-5','S1553-4650(17)30469-7','S1553-4650(17)30470-3',
  'S1553-4650(17)30471-5','S1553-4650(17)30472-7','S1553-4650(17)30473-9',
  'S1553-4650(17)30474-0','S1553-4650(17)30475-2','S1553-4650(17)30476-4',
  'S1553-4650(17)30477-6','S1553-4650(17)30478-8','S1553-4650(17)30479-X',
  'S1553-4650(17)30480-6','S1553-4650(17)30481-8','S1553-4650(17)30482-X',
  'S1553-4650(17)30483-1','S1553-4650(17)30484-3','S1553-4650(17)30485-5',
  'S1553-4650(17)30486-7','S1553-4650(17)30487-9','S1553-4650(17)30488-0',
  'S1553-4650(17)30489-2','S1553-4650(17)30490-9','S1553-4650(17)30491-0',
  'S1553-4650(17)30492-2','S1553-4650(17)30493-4','S1553-4650(17)30494-6',
  'S1553-4650(17)30495-8','S1553-4650(17)30496-X','S1553-4650(17)30497-1',
  'S1553-4650(17)30498-3','S1553-4650(17)30499-5','S1553-4650(17)30500-9',
  'S1553-4650(17)30501-0','S1553-4650(17)30502-2','S1553-4650(17)30506-X',
  'S1553-4650(17)30507-1','S1553-4650(17)30508-3','S1553-4650(17)30509-5',
  'S1553-4650(17)30510-1','S1553-4650(17)30511-3','S1553-4650(17)30512-5',
  'S1553-4650(17)30513-7','S1553-4650(17)30515-0','S1553-4650(17)30516-2',
  'S1553-4650(17)30517-4','S1553-4650(17)30518-6','S1553-4650(17)30519-8',
  'S1553-4650(17)30520-4','S1553-4650(17)30521-6','S1553-4650(17)30522-8',
  'S1553-4650(17)30523-X','S1553-4650(17)30524-1','S1553-4650(17)30525-3',
  'S1553-4650(17)30526-5','S1553-4650(17)30527-7','S1553-4650(17)30528-9',
  'S1553-4650(17)30529-0','S1553-4650(17)30530-7','S1553-4650(17)30531-9',
  'S1553-4650(17)30532-0','S1553-4650(17)30533-2','S1553-4650(17)30534-4',
  'S1553-4650(17)30535-6','S1553-4650(17)30536-8','S1553-4650(17)30537-X',
  'S1553-4650(17)30538-1','S1553-4650(17)30539-3','S1553-4650(17)30540-X',
  'S1553-4650(17)30541-1','S1553-4650(17)30542-3','S1553-4650(17)30543-5',
  'S1553-4650(17)30544-7','S1553-4650(17)30545-9','S1553-4650(17)30546-0',
  'S1553-4650(17)30547-2','S1553-4650(17)30548-4','S1553-4650(17)30549-6',
  'S1553-4650(17)30550-2','S1553-4650(17)30551-4','S1553-4650(17)30552-6',
  'S1553-4650(17)30553-8'
];

function extractText(doc) {
  // Try outline / abstract sections first
  const selectors = [
    '.article-outline-content',
    '[class*="outline-content"]',
    '.article__body .body',
    'section.body',
    '#abstracts',
  ];
  for (const sel of selectors) {
    const el = doc.querySelector(sel);
    if (el && el.innerText && el.innerText.trim().length > 30)
      return el.innerText.trim().substring(0, 800);
  }
  // Fallback: grab all paragraphs
  const paras = [...doc.querySelectorAll('p')]
    .map(p => p.innerText.trim())
    .filter(t => t.length > 30 && !t.includes('cookie') && !t.includes('login'));
  return paras.slice(0, 5).join(' ').substring(0, 800);
}

let results = [];
let done = 0;

async function scrapeAll() {
  console.log(`Starting scrape of ${piis.length} abstracts...`);
  for (const pii of piis) {
    const url = `/article/${pii}/abstract`;
    try {
      const resp = await fetch(url, {credentials: 'include'});
      const html = await resp.text();
      const doc = new DOMParser().parseFromString(html, 'text/html');
      const text = extractText(doc);
      results.push({pii, url: `https://www.jmig.org${url}`, abstract_text: text, status: resp.status});
    } catch(e) {
      results.push({pii, url: `https://www.jmig.org${url}`, abstract_text: null, error: e.message});
    }
    done++;
    if (done % 10 === 0) console.log(`Progress: ${done}/${piis.length}`);
    await new Promise(r => setTimeout(r, 600)); // polite delay
  }

  console.log(`Done! Got text for ${results.filter(r => r.abstract_text && r.abstract_text.length > 30).length}/${piis.length} abstracts`);

  // Download JSON
  const json = JSON.stringify(results, null, 2);
  const blob = new Blob([json], {type: 'application/json'});
  const a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = 'jmig_2017_abstracts.json';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  console.log('Downloaded: jmig_2017_abstracts.json');
  return results;
}

scrapeAll();
