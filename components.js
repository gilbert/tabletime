import s from 'cofound'
import {
  BOARD_SIZE,
  CARD_PORTRAIT_HEIGHT,
  CARD_PORTRAIT_WIDTH,
  CELL_SIZE,
  CHIP_SIZE
} from './constants.js'

let globalStylesInstalled = false

export function installGlobalStyles() {
  if (globalStylesInstalled) return
  globalStylesInstalled = true

  s.css.reset`
    body {
      background: #16191d;
      color: #f5f1e8;
    }

    button {
      border: 0;
    }
  `

  s.css`
    :root {
      color-scheme: dark;
      --felt: #1f6d4a;
      --felt-dark: #145239;
      --ink: #20242a;
      --paper: #fbf4df;
      --muted: #9ba6b2;
      --line: rgba(255,255,255,.16);
      --shadow: 0 18px 50px rgba(0,0,0,.32);
    }

    html,
    body {
      width: 100%;
      height: 100%;
      overflow: hidden;
    }
  `
}

export const AppShell = s`main
  display grid
  grid-template-rows 58px minmax(0, 1fr) 190px
  width 100vw
  height 100svh
  background #16191d
`

export const MainContent = s`section
  display grid
  grid-template-columns 300px minmax(0, 1fr)
  min-width 0
  min-height 0
  overflow hidden
  background #16191d

  @media (max-width: 820px) {
    grid-template-columns 260px minmax(0, 1fr)
  }
`

export const TopBar = s`header
  display flex
  align-items center
  justify-content space-between
  gap 18px
  width 100vw
  overflow hidden
  padding 0 18px
  border-bottom 1px solid rgba(255,255,255,.1)
  background #20242a
  box-shadow 0 8px 24px rgba(0,0,0,.22)
  z-index 10
`

export const Brand = s`div
  display flex
  align-items center
  gap 12px
  flex 0 0 auto
`

export const Mark = s`div
  width 30px
  height 30px
  border-radius 7px
  background linear-gradient(135deg, #d9b35f, #a94747 48%, #286caa)
  box-shadow inset 0 0 0 1px rgba(255,255,255,.24)
`

export const TitleBlock = s`div
  display grid
  gap 2px
`

export const Title = s`h1
  font-size 15px
  line-height 1
  font-weight 750
  letter-spacing 0
`

export const Subtitle = s`p
  font-size 12px
  line-height 1.2
  color var(--muted)
`

export const StatusStrip = s`div
  display flex
  align-items center
  justify-content flex-end
  gap 10px
  min-width 0
  overflow-x auto
  scrollbar-width none

  &::-webkit-scrollbar {
    display none
  }

  @media (max-width: 560px) {
    justify-content flex-start
    gap 8px
  }
`

export const Pill = s`span
  display inline-flex
  align-items center
  gap 7px
  min-height 30px
  padding 0 10px
  border 1px solid rgba(255,255,255,.12)
  border-radius 7px
  background rgba(255,255,255,.06)
  color #e9edf1
  font-size 12px
  white-space nowrap
  flex 0 0 auto

  @media (max-width: 560px) {
    padding 0 9px
    font-size 11px
  }
`

export const PlayerDot = s`span
  display inline-block
  width 10px
  height 10px
  border-radius 50%
  box-shadow 0 0 0 2px rgba(255,255,255,.18)
`

export const ToolbarButton = s`button
  min-height 34px
  padding 0 12px
  border-radius 7px
  background #f1d28a
  color #24201a
  font-size 13px
  font-weight 750
  cursor pointer

  &:hover {
    background #ffe09b
  }

  &:active {
    transform translateY(1px)
  }

  &:disabled {
    cursor default
    opacity .5
    transform none
  }

  @media (max-width: 560px) {
    min-height 32px
    padding 0 9px
    font-size 12px
  }
`

export const SecondaryButton = ToolbarButton`
  background rgba(255,255,255,.1)
  color #f5f1e8
  border 1px solid rgba(255,255,255,.12)

  &:hover {
    background rgba(255,255,255,.16)
  }

  &:disabled {
    cursor default
    opacity .5
  }
`

export const Workspace = s`section
  position relative
  min-width 0
  min-height 0
  overflow auto
  background
    radial-gradient(circle at 20% 20%, rgba(255,255,255,.08), transparent 28%),
    linear-gradient(135deg, #1b5d41, #16513a 48%, #124434)
`

export const TableSurface = s`div
  position relative
  width var(--table-width)
  height var(--table-height)
  background
    linear-gradient(rgba(255,255,255,.03) 1px, transparent 1px),
    linear-gradient(90deg, rgba(255,255,255,.03) 1px, transparent 1px),
    radial-gradient(circle at 50% 40%, rgba(255,255,255,.07), transparent 45%),
    var(--felt)
  background-size 80px 80px, 80px 80px, auto, auto
  box-shadow inset 0 0 0 18px rgba(15, 52, 38, .72), inset 0 0 120px rgba(0,0,0,.35)
`

export const PresenceCursor = s`div
  position absolute
  z-index 70
  display grid
  grid-template-columns 0 auto
  align-items start
  pointer-events none
  transform translate(2px, 2px)
  transition left 70ms linear, top 70ms linear
  will-change left, top

  &::before {
    content ''
    width 0
    height 0
    border-left 9px solid var(--presence-color)
    border-top 7px solid transparent
    border-bottom 7px solid transparent
    filter drop-shadow(0 2px 4px rgba(0,0,0,.35))
    transform rotate(38deg)
  }

  span {
    margin-left 10px
    margin-top 9px
    padding 3px 6px
    border-radius 6px
    background var(--presence-color)
    color white
    font-size 11px
    font-weight 800
    box-shadow 0 5px 14px rgba(0,0,0,.28)
    white-space nowrap
  }
`

export const TableObjectShell = s`section
  position absolute
  z-index 2
  border 1px solid rgba(255,255,255,.16)
  border-radius 8px
  background rgba(32,36,42,.9)
  box-shadow var(--shadow)
  overflow hidden
  user-select none
  cursor default
`

export const ObjectHeader = s`header
  display flex
  align-items center
  justify-content space-between
  gap 10px
  height 34px
  padding 0 10px
  border-bottom 1px solid rgba(255,255,255,.1)
  background rgba(255,255,255,.06)
  color #e8edf1
  font-size 12px
  font-weight 750
  touch-action none
`

export const LockBadge = s`span
  color var(--muted)
  font-size 11px
  font-weight 650
`

export const BoardWrap = s`div
  padding 12px
`

export const BoardGrid = s`div
  display grid
  grid-template-columns repeat(var(--board-size, ${BOARD_SIZE}), var(--cell-size, ${CELL_SIZE + 'px'}))
  grid-template-rows repeat(var(--board-size, ${BOARD_SIZE}), var(--cell-size, ${CELL_SIZE + 'px'}))
  gap var(--board-gap, 4px)
`

export const BoardCell = s`div
  position relative
  display grid
  place-items center
  width var(--cell-size, ${CELL_SIZE + 'px'})
  height var(--cell-size, ${CELL_SIZE + 'px'})
  border 1px solid rgba(44, 51, 59, .22)
  border-radius 6px
  background var(--paper)
  color var(--ink)
  box-shadow inset 0 0 0 1px rgba(255,255,255,.38)
  cursor default

  &[data-free="true"] {
    background #d7b365
  }

  &[data-board-kind="blokus"] {
    border-radius 2px
    border-color rgba(255,255,255,.16)
    background #e8edf1
    box-shadow inset 0 0 0 1px rgba(31,45,58,.08)
  }

  &[data-board-kind="blokus"][data-corner="true"] {
    background #f1d28a
  }
`

export const CardFace = s`div
  display grid
  place-items center
  gap 2px
  width 100%
  height 100%
  padding 6px
`

export const CardRank = s`strong
  font-size 18px
  line-height 1
  letter-spacing 0
`

export const CardSuit = s`span
  font-size 14px
  line-height 1
  font-weight 800
`

export const TableChip = s`button
  position absolute
  z-index 5
  width ${CHIP_SIZE + 'px'}
  height ${CHIP_SIZE + 'px'}
  border-radius 50%
  background var(--chip-color)
  border 3px solid rgba(255,255,255,.72)
  box-shadow 0 4px 10px rgba(0,0,0,.35), inset 0 -5px 8px rgba(0,0,0,.22), inset 0 5px 8px rgba(255,255,255,.22)
  cursor grab
  touch-action none

  &:active {
    cursor grabbing
  }

  &[data-dragging="true"] {
    transform scale(1.06)
    box-shadow 0 12px 24px rgba(0,0,0,.4), inset 0 -5px 8px rgba(0,0,0,.22), inset 0 5px 8px rgba(255,255,255,.22)
  }

  &[data-selected="true"] {
    outline 3px solid #f1d28a
    outline-offset 3px
  }

  &[data-locked="true"] {
    cursor default
    filter saturate(.75)
  }

  &:disabled {
    cursor default
    filter saturate(.75)
  }
`

export const PolyominoPieceButton = s`button
  position absolute
  z-index 7
  display block
  width var(--piece-width)
  height var(--piece-height)
  border 0
  background transparent
  padding 0
  cursor grab
  touch-action none

  &:active {
    cursor grabbing
  }

  &[data-selected="true"] {
    filter drop-shadow(0 0 0 #f1d28a) drop-shadow(0 0 8px rgba(241,210,138,.85))
  }

  &[data-locked="true"] {
    cursor default
    filter saturate(.7)
  }

  &:disabled {
    cursor default
    filter saturate(.7)
  }
`

export const PolyominoShape = s`div
  position relative
  width 100%
  height 100%
`

export const PolyominoCell = s`span
  position absolute
  left calc(var(--cell-x) * var(--piece-cell-size))
  top calc(var(--cell-y) * var(--piece-cell-size))
  width var(--piece-cell-size)
  height var(--piece-cell-size)
  border 1px solid rgba(255,255,255,.62)
  border-radius 4px
  background var(--piece-color)
  box-shadow inset 0 -4px 7px rgba(0,0,0,.2), inset 0 3px 6px rgba(255,255,255,.22), 0 4px 10px rgba(0,0,0,.24)
  transition left 180ms cubic-bezier(.2,.8,.2,1), top 180ms cubic-bezier(.2,.8,.2,1)
`

export const TableCardButton = s`button
  position absolute
  z-index 6
  display grid
  width var(--piece-width)
  height var(--piece-height)
  border 1px solid rgba(37,42,49,.22)
  border-radius 8px
  background var(--paper)
  color var(--ink)
  box-shadow 0 16px 30px rgba(0,0,0,.34)
  cursor grab
  touch-action none
  transform rotate(var(--piece-rotate))
  transform-origin 50% 50%

  &:active {
    cursor grabbing
  }

  &[data-selected="true"] {
    outline 3px solid #f1d28a
    outline-offset 3px
  }

  &[data-locked="true"] {
    cursor default
    filter saturate(.7)
  }

  &:disabled {
    cursor default
    filter saturate(.7)
  }

  &[data-dragging="true"] {
    z-index 60
  }
`

export const CardBack = s`div
  position relative
  display grid
  place-items center
  width 100%
  height 100%
  border-radius 7px
  overflow hidden
  background-color #223c4c
  background-image url('/tabletime-playing-card-back.png')
  background-size cover
  background-position center
  color transparent
  font-size 0
`

export const FreeCorner = s`span
  display grid
  place-items center
  width 42px
  height 42px
  border-radius 50%
  border 2px dashed rgba(45,42,32,.42)
  color rgba(45,42,32,.72)
  font-size 11px
  font-weight 850
`

export const DeckBody = s`div
  display grid
  gap 12px
  width 180px
  padding 14px
`

export const DeckStack = s`div
  position relative
  display grid
  place-items center
  width 112px
  height 156.8px
  margin 0 auto
  border-radius 7px
  background #29313a
  border 2px solid rgba(255,255,255,.16)
  box-shadow 7px 7px 0 #1d242b, 13px 13px 0 #151b21
  color #f5f1e8
  font-size 13px
  font-weight 800
`

export const StackCount = s`span
  display grid
  place-items center
  width 54px
  height 54px
  border-radius 50%
  background #f1d28a
  color #24201a
  box-shadow 0 8px 20px rgba(0,0,0,.32)
`

export const ObjectActions = s`div
  display grid
  grid-template-columns 1fr 1fr
  gap 8px
`

export const MiniButton = s`button
  height 32px
  min-width 58px
  padding 0 12px
  border-radius 6px
  background rgba(255,255,255,.1)
  color #f5f1e8
  border 1px solid rgba(255,255,255,.14)
  cursor pointer
  font-size 12px
  font-weight 750

  &:hover {
    background rgba(255,255,255,.16)
  }

  &:disabled {
    cursor default
    opacity .5
  }
`

export const AuthOverlay = s`section
  position fixed
  inset 0
  z-index 200
  display grid
  place-items center
  padding 18px
  background rgba(13,16,20,.74)
  backdrop-filter blur(8px)
`

export const AuthPanel = s`form
  display grid
  gap 12px
  width min(360px, 100%)
  padding 18px
  border 1px solid rgba(255,255,255,.16)
  border-radius 8px
  background #20242a
  box-shadow var(--shadow)
`

export const AuthField = s`input
  width 100%
  height 38px
  padding 0 10px
  border 1px solid rgba(255,255,255,.18)
  border-radius 7px
  background rgba(255,255,255,.08)
  color #f5f1e8
  font-size 14px
  outline none

  &:focus {
    border-color #f1d28a
  }
`

export const FormNote = s`p
  color var(--muted)
  font-size 12px
  line-height 1.35
`

export const FormError = FormNote`
  color #ffb6aa
`

export const DiscardBody = s`div
  display grid
  place-items center
  gap 10px
  width 168px
  padding 14px

  &[data-drop-ready="true"] {
    background rgba(241,210,138,.12)
    box-shadow inset 0 0 0 2px rgba(241,210,138,.7)
  }
`

export const DiscardCardButton = s`button
  display grid
  width ${CARD_PORTRAIT_WIDTH + 'px'}
  height ${CARD_PORTRAIT_HEIGHT + 'px'}
  border 1px solid rgba(37,42,49,.22)
  border-radius 8px
  background var(--paper)
  color var(--ink)
  box-shadow 0 14px 28px rgba(0,0,0,.3)
  cursor grab
  touch-action none

  &:active {
    cursor grabbing
  }

  &:hover {
    box-shadow 0 22px 38px rgba(0,0,0,.38)
  }

  &:disabled {
    cursor default
  }

  &[selected] {
    outline 3px solid #f1d28a
    outline-offset 3px
  }
`

export const SupplyBody = s`div
  display grid
  gap 12px
  width 180px
  padding 14px
`

export const SupplyRow = s`div
  display flex
  align-items center
  justify-content space-between
  gap 10px
  height 38px
  padding 0 10px
  border-radius 7px
  background rgba(255,255,255,.08)
  color #f5f1e8
  cursor default
  font-size 13px
  font-weight 750
`

export const SupplyChip = s`span
  display inline-block
  width 22px
  height 22px
  border-radius 50%
  border 2px solid rgba(255,255,255,.72)
  box-shadow 0 3px 8px rgba(0,0,0,.3), inset 0 -4px 6px rgba(0,0,0,.2), inset 0 4px 6px rgba(255,255,255,.18)
  cursor grab
  touch-action none

  &[data-disabled="true"] {
    cursor default
    filter saturate(.75)
  }
`

export const SidePanel = s`aside
  position relative
  z-index 8
  display grid
  grid-template-rows auto minmax(0, 1fr)
  width 300px
  min-height 0
  border 1px solid rgba(255,255,255,.14)
  border-top 0
  border-left 0
  border-bottom 0
  border-radius 0
  background rgba(32,36,42,.92)
  box-shadow var(--shadow)
  overflow hidden

  @media (max-width: 820px) {
    width 260px
  }
`

export const PanelSection = s`section
  padding 12px
  border-top 1px solid rgba(255,255,255,.08)
  min-height 0

  &:first-child {
    border-top 0
  }
`

export const LogPanelSection = PanelSection`
  display grid
  grid-template-rows auto minmax(0, 1fr)
  overflow hidden
`

export const PanelTitle = s`h2
  margin-bottom 8px
  color #f5f1e8
  font-size 12px
  line-height 1
  font-weight 850
`

export const MetricGrid = s`div
  display grid
  grid-template-columns 1fr 1fr
  gap 8px
`

export const Metric = s`div
  display grid
  gap 4px
  padding 9px
  border-radius 7px
  background rgba(255,255,255,.07)
`

export const MetricValue = s`strong
  font-size 18px
  line-height 1
`

export const MetricLabel = s`span
  color var(--muted)
  font-size 11px
  line-height 1.2
`

export const SeatList = s`div
  display grid
  gap 8px
  margin-top 12px
`

export const SeatRow = s`div
  display grid
  grid-template-columns 18px minmax(0, 1fr) auto
  align-items center
  gap 9px
  min-height 42px
  padding 8px
  border-radius 7px
  background rgba(255,255,255,.07)
`

export const SeatSwatch = s`span
  width 14px
  height 14px
  border-radius 50%
  background var(--seat-color)
  box-shadow 0 0 0 2px rgba(255,255,255,.18)
`

export const SeatText = s`div
  display grid
  gap 3px
  min-width 0
`

export const SeatName = s`strong
  color #f5f1e8
  font-size 12px
  line-height 1
  font-weight 800
  white-space nowrap
  overflow hidden
  text-overflow ellipsis
`

export const SeatOccupant = s`span
  color var(--muted)
  font-size 11px
  line-height 1.1
  white-space nowrap
  overflow hidden
  text-overflow ellipsis
`

export const PanelActions = s`div
  display grid
  grid-template-columns 1fr
  gap 8px
  margin-top 10px
`

export const LogList = s`ol
  display flex
  flex-direction column
  gap 7px
  min-height 0
  overflow auto
  padding-right 4px
  scrollbar-width thin
`

export function autoscrollLog(dom) {
  const scrollToBottom = () => {
    dom.scrollTop = dom.scrollHeight
  }
  scrollToBottom()

  const Observer = globalThis.MutationObserver
  if (!Observer) return undefined

  const observer = new Observer(scrollToBottom)
  observer.observe(dom, { childList: true, subtree: true })
  return () => observer.disconnect()
}

export const LogItem = s`li
  color #cdd5dd
  font-size 12px
  line-height 1.3
`

export const HandBar = s`section
  position relative
  z-index 12
  width 100vw
  overflow visible
  border-top 1px solid rgba(255,255,255,.1)
  background #20242a
  box-shadow 0 -10px 28px rgba(0,0,0,.24)
`

export const HandStatus = s`div
  position absolute
  left 18px
  top 16px
  width 205px
`

export const HandIdentity = s`div
  display grid
  grid-template-columns auto minmax(0, 1fr)
  align-items start
  gap 9px
`

export const HandSeatSwatch = SeatSwatch`
  width 16px
  height 16px
  margin-top 1px
  box-shadow 0 0 0 2px rgba(255,255,255,.22), 0 6px 14px rgba(0,0,0,.22)
`

export const HandCards = s`div
  position absolute
  left 50%
  bottom 14px
  width min(760px, calc(100vw - 36px))
  height 160px
  transform translateX(-50%)
  overflow visible

  @media (max-width: 560px) {
    bottom 4px
    height 124px
  }
`

export const HandCardButton = s`button
  position absolute
  bottom 0
  width ${CARD_PORTRAIT_WIDTH + 'px'}
  height ${CARD_PORTRAIT_HEIGHT + 'px'}
  border 1px solid rgba(37,42,49,.22)
  border-radius 8px
  background var(--paper)
  color var(--ink)
  box-shadow 0 14px 28px rgba(0,0,0,.3)
  cursor grab
  touch-action none
  transform-origin 50% 140%
  transition transform 140ms, box-shadow 140ms

  &:active {
    cursor grabbing
  }

  &:hover {
    transform translateY(calc(var(--card-lift) - 18px)) rotate(var(--card-rotate))
    box-shadow 0 22px 38px rgba(0,0,0,.38)
  }

  &[selected] {
    outline 3px solid #f1d28a
    outline-offset 3px
    transform translateY(calc(var(--card-lift) - 24px)) rotate(var(--card-rotate))
  }

  &[data-dragging="true"] {
    opacity .28
  }

  &[data-entering-draw="true"] {
    opacity 0
  }
`

export const HandCardInner = s`div
  display grid
  grid-template-rows auto 1fr auto
  width 100%
  height 100%
  padding 10px
`

export const CornerText = s`span
  justify-self start
  display grid
  gap 2px
  font-size 15px
  line-height 1
  font-weight 850
`

export const CenterRank = s`strong
  place-self center
  font-size 32px
  line-height 1
  letter-spacing 0
`

export const BottomText = CornerText`
  justify-self end
  transform rotate(180deg)
`

export const ActionRail = s`div
  position absolute
  right 18px
  top 16px
  display flex
  gap 8px

  @media (max-width: 560px) {
    right 8px
    top 14px
    gap 6px
    flex-direction column
  }
`

export const RemoteHandsLayer = s`div
  position absolute
  inset 0
  z-index 4
  pointer-events none
`

export const RemoteHandZone = s`section
  position absolute
  display grid
  justify-items center
  gap 8px
  min-width 190px
  padding 8px

  &[data-position="top"] {
    left 50%
    top 20px
    transform translateX(-50%)
  }

  &[data-position="left"] {
    left 24px
    top 50%
    transform translateY(-50%)
  }

  &[data-position="right"] {
    right 24px
    top 50%
    transform translateY(-50%)
  }
`

export const RemoteHandLabel = s`div
  display inline-flex
  align-items center
  gap 7px
  max-width 190px
  min-height 28px
  padding 0 9px
  border 1px solid rgba(255,255,255,.14)
  border-radius 7px
  background rgba(22,25,29,.72)
  color #f5f1e8
  font-size 12px
  font-weight 750
  box-shadow 0 8px 20px rgba(0,0,0,.22)
  white-space nowrap
  overflow hidden
  text-overflow ellipsis

  &::before {
    content ''
    width 9px
    height 9px
    border-radius 50%
    background var(--seat-color)
    box-shadow 0 0 0 2px rgba(255,255,255,.18)
    flex 0 0 auto
  }
`

export const RemoteHandCards = s`div
  display flex
  justify-content center
  align-items center
  min-height 72px
  padding-left 18px
`

export const RemoteHandBack = s`div
  position relative
  display grid
  place-items center
  width 48px
  height 67.2px
  margin-left -18px
  overflow hidden
  border 1px solid rgba(255,255,255,.34)
  border-radius 7px
  background-color #223c4c
  background-image url('/tabletime-playing-card-back.png')
  background-size cover
  background-position center
  box-shadow 0 8px 18px rgba(0,0,0,.38), inset 0 0 0 2px rgba(255,255,255,.08)
`

export const RemoteHandDragCard = s`div
  position absolute
  z-index 65
  display grid
  border 1px solid rgba(255,255,255,.34)
  border-radius 8px
  background #29313a
  box-shadow 0 24px 44px rgba(0,0,0,.42), 0 0 0 3px color-mix(in srgb, var(--presence-color) 46%, transparent)
  pointer-events none
  overflow hidden
  opacity .96
  transition left 45ms linear, top 45ms linear
  will-change left, top

  &[data-returning="true"] {
    opacity .9
    transition left 190ms cubic-bezier(.2,.8,.2,1), top 190ms cubic-bezier(.2,.8,.2,1), opacity 190ms
  }
`
